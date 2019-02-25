open Protocols
open Core
open Async
open Protocols.Coda_transition_frontier
open Coda_base

module type Inputs_intf = sig
  include Transition_frontier.Inputs_intf

  module Transition_frontier :
    Transition_frontier_intf
    with type state_hash := State_hash.t
     and type external_transition_verified := External_transition.Verified.t
     and type ledger_database := Ledger.Db.t
     and type staged_ledger := Staged_ledger.t
     and type masked_ledger := Ledger.Mask.Attached.t
     and type transaction_snark_scan_state := Staged_ledger.Scan_state.t
     and type staged_ledger_diff := Staged_ledger_diff.t
     and type consensus_local_state := Consensus.Local_state.t
     and type user_command := User_command.t

  module Time : Protocols.Coda_pow.Time_intf

  module Protocol_state_validator :
    Protocol_state_validator_intf
    with type time := Time.t
     and type state_hash := State_hash.t
     and type external_transition := External_transition.t
     and type external_transition_proof_verified :=
                External_transition.Proof_verified.t
     and type external_transition_verified := External_transition.Verified.t
end

module Make (Inputs : Inputs_intf) :
  Coda_transition_frontier.Root_prover_intf
  with type state_body_hash := State_body_hash.t
   and type transition_frontier := Inputs.Transition_frontier.t
   and type external_transition := Inputs.External_transition.t
   and type proof_verified_external_transition :=
              Inputs.External_transition.Proof_verified.t
   and type consensus_state := Consensus.Consensus_state.value
   and type state_hash := State_hash.t = struct
  open Inputs

  let hash_transition =
    Fn.compose Consensus.Protocol_state.hash External_transition.protocol_state

  let get_state_hash transition_with_hash =
    let open External_transition in
    transition_with_hash |> With_hash.data |> Verified.protocol_state
    |> Protocol_state.body |> Protocol_state.Body.hash

  let consensus_state transition =
    External_transition.(
      protocol_state transition |> Protocol_state.consensus_state)

  let prove ~logger ~frontier seen_consensus_state :
      ( External_transition.t
      , State_body_hash.t List.t * External_transition.t )
      Proof_carrying_data.t
      option =
    let open Option.Let_syntax in
    let%bind () =
      Option.some_if
        ( Transition_frontier.best_tip_path_length_exn frontier
        = Transition_frontier.max_length )
        ()
    in
    let best_tip_breadcrumb = Transition_frontier.best_tip frontier in
    let best_tip =
      Transition_frontier.Breadcrumb.transition_with_hash best_tip_breadcrumb
      |> With_hash.data |> External_transition.of_verified
    in
    let is_tip_better =
      Consensus.select ~logger ~existing:(consensus_state best_tip)
        ~candidate:seen_consensus_state
      = `Keep
    in
    let%bind () = Option.some_if is_tip_better () in
    let exclusive_merkle_list =
      Transition_frontier.path_map frontier best_tip_breadcrumb
        ~f:(fun breadcrumb ->
          let transition_with_hash =
            Transition_frontier.Breadcrumb.transition_with_hash breadcrumb
          in
          Debug_assert.debug_assert (fun () ->
              let state_hash = With_hash.hash transition_with_hash in
              Logger.info logger
                !"Hash of a breadcrumb in path: %{sexp:State_hash.t}"
                state_hash ) ;
          get_state_hash transition_with_hash )
    in
    let root_with_hash =
      Transition_frontier.root frontier
      |> Transition_frontier.Breadcrumb.transition_with_hash
    in
    Logger.info logger
      !"Produced a merkle list of %{sexp:State_body_hash.t list}"
      exclusive_merkle_list ;
    Some
      Proof_carrying_data.
        { data=
            root_with_hash |> With_hash.data |> External_transition.of_verified
        ; proof= (exclusive_merkle_list, best_tip) }

  let verify_merkle_list ~logger root_state_hash merkle_list best_tip_hash =
    let result_hash =
      List.fold merkle_list ~init:root_state_hash ~f:(fun acc body_hash ->
          let state_hash =
            Protocol_state.hash ~hash_body:Fn.id
              {previous_state_hash= acc; body= body_hash}
          in
          Logger.info logger
            !"State_hash produce %{sexp:State_hash.t}"
            state_hash ;
          state_hash )
    in
    State_hash.equal best_tip_hash result_hash

  let check_error ~message cond =
    let open Deferred.Or_error in
    if cond then return () else error_string message

  let verify ~logger ~observed_state
      ~peer_root:{Proof_carrying_data.data= root; proof= merkle_list, best_tip}
      =
    let open Deferred.Result.Let_syntax in
    let merkle_list_length = List.length merkle_list in
    let root_transition_with_hash =
      With_hash.of_data root ~hash_data:hash_transition
    in
    let root_hash = With_hash.hash root_transition_with_hash in
    let%bind () =
      check_error
        ~message:
          (sprintf
             !"Peer should have given a proof of length %d but got %d"
             Transition_frontier.max_length merkle_list_length)
        (Int.equal Transition_frontier.max_length merkle_list_length)
    in
    let best_tip_with_hash =
      With_hash.of_data best_tip ~hash_data:hash_transition
    in
    let best_tip_hash = With_hash.hash best_tip_with_hash in
    (* This statement might not see a peer's best_tip as the best_tip *)
    let is_before_best_tip candidate =
      Consensus.select ~logger ~existing:(consensus_state best_tip) ~candidate
      = `Keep
    in
    let%bind () =
      check_error
        ~message:
          (sprintf
             !"Peer lied about it's best tip %{sexp:State_hash.t}"
             best_tip_hash)
        (is_before_best_tip observed_state)
    in
    let%bind () =
      check_error ~message:"Peer gave an invalid proof of it's root"
        (verify_merkle_list ~logger root_hash merkle_list best_tip_hash)
    in
    let%bind validated_root = Protocol_state_validator.validate_proof root in
    let%map validated_best_tip =
      Protocol_state_validator.validate_proof best_tip
    in
    ( {With_hash.data= validated_root; hash= root_hash}
    , {With_hash.data= validated_best_tip; hash= best_tip_hash} )
end
