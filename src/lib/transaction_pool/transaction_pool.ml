open Core_kernel
open Async_kernel
open Protocols.Coda_transition_frontier
open Pipe_lib

module type Transition_frontier_intf = sig
  type t

  type user_command

  module Breadcrumb : sig
    type t [@@deriving sexp]

    val to_user_commands : t -> user_command list
  end

  module Extensions : sig
    module Best_tip_diff : sig
      type view = Breadcrumb.t Best_tip_diff_view.t Option.t
    end

    type readers

    val best_tip_diff : readers -> Best_tip_diff.view Broadcast_pipe.Reader.t
  end

  val extension_pipes : t -> Extensions.readers
end

(*
 * TODO: Remove could be really slow, we need to deal with this:
 *
 *  Reification of in-person discussion:
 *  Let's say our transaction pool has 100M transactions in it
 *  The question is: How often will we be removing transactions?
 *
 * 1. If we want to minimize space, we can remove transactions as soon as we
 *    see that they were used. In this case, we shouldn't use an Fheap as
 *    removing is O(n). We could use a balanced BST instead and remove would be
 *    faster, but we'd sacrifice `get` performance.
 * 2. We could instead just pop from our heap until we get `k` transactions that
 *    are valid on the current state (optionally we could use periodic garbage
 *    collection as well).
 *
 * For now we are removing lazily when we look for the next transactions
 *)
module Make (User_command : sig
  type t [@@deriving compare, bin_io, sexp]

  module With_valid_signature : sig
    type nonrec t = private t [@@deriving sexp]

    include Comparable with type t := t
  end

  val check : t -> With_valid_signature.t option
end)
(Transition_frontier : Transition_frontier_intf
                       with type user_command := User_command.t) =
struct
  module Breadcrumb = Transition_frontier.Breadcrumb

  type pool =
    { heap: User_command.With_valid_signature.t Fheap.t
    ; set: User_command.With_valid_signature.Set.t }

  type t =
    { mutable pool: pool
    ; log: Logger.t
    ; mutable diff_reader: unit Deferred.t Option.t }

  type transition_frontier = Transition_frontier.t

  (* FIXME terrible hack *)
  let remove_tx t tx =
    t.pool
    <- { heap=
           Fheap.of_list ~cmp:User_command.With_valid_signature.compare
             (List.filter (Fheap.to_list t.pool.heap) ~f:(fun tx' ->
                  User_command.With_valid_signature.compare tx tx' = 0 ))
       ; set= User_command.With_valid_signature.Set.remove t.pool.set tx }

  let handle_diff t
      (diff_opt : Transition_frontier.Extensions.Best_tip_diff.view) =
    match diff_opt with
    | None ->
        Logger.debug t.log "Got empty best tip diff" ;
        Deferred.unit
    | Some {old_best_tip; new_best_tip} ->
        let new_user_commands = Breadcrumb.to_user_commands new_best_tip in
        Logger.debug t.log
          !"Diff: old: %{sexp:User_command.t list} new: %{sexp:User_command.t \
            list}"
          (Breadcrumb.to_user_commands old_best_tip)
          new_user_commands ;
        List.iter new_user_commands ~f:(fun tx -> () (*remove_tx t tx*)) ;
        Deferred.unit

  let create ~parent_log ~frontier_broadcast_pipe =
    let t =
      { pool=
          { heap= Fheap.create ~cmp:User_command.With_valid_signature.compare
          ; set= User_command.With_valid_signature.Set.empty }
      ; log= Logger.child parent_log __MODULE__
      ; diff_reader= None }
    in
    don't_wait_for
      ( fst
      @@ Broadcast_pipe.Reader.iter frontier_broadcast_pipe
           ~f:(fun frontier_opt ->
             match frontier_opt with
             | None ->
                 Logger.debug t.log "no frontier" ;
                 (* Sanity check: the view pipe should have been closed before
                    the frontier was destroyed. *)
                 (* FIXME this is wrong, closing pipes is async, need to wait. *)
                 [%test_eq: bool option]
                   (Option.map t.diff_reader ~f:Deferred.is_determined)
                   (Some true) ;
                 Deferred.unit (* TODO more cleanup logic here? *)
             | Some frontier ->
                 Logger.debug t.log "Got frontier!\n" ;
                 (* TODO check current pool contents are valid against best tip here *)
                 t.diff_reader
                 <- Some
                      ( fst
                      @@ Broadcast_pipe.Reader.iter
                           ( Transition_frontier.extension_pipes frontier
                           |> Transition_frontier.Extensions.best_tip_diff )
                           ~f:(handle_diff t) ) ;
                 Deferred.unit ) ) ;
    t

  let add' pool txn = {heap= Fheap.add pool.heap txn; set= Set.add pool.set txn}

  let add t txn = t.pool <- add' t.pool txn

  let transactions t = Sequence.unfold ~init:t.pool.heap ~f:Fheap.pop

  module Diff = struct
    type t = User_command.t list [@@deriving bin_io, sexp]

    let summary t =
      Printf.sprintf "Transaction diff of length %d" (List.length t)

    (* TODO: Check signatures *)
    let apply t env =
      let txns = Envelope.Incoming.data env in
      let pool0 = t.pool in
      let pool', res =
        List.fold txns ~init:(pool0, []) ~f:(fun (pool, acc) txn ->
            match User_command.check txn with
            | None ->
                Logger.faulty_peer t.log
                  !"Transaction doesn't check %{sexp: Network_peer.Peer.t}"
                  (Envelope.Incoming.sender env) ;
                (pool, acc)
            | Some txn ->
                if Set.mem pool.set txn then (
                  Logger.debug t.log
                    !"Skipping txn %{sexp: \
                      User_command.With_valid_signature.t} because I've \
                      already seen it"
                    txn ;
                  (pool, acc) )
                else (
                  Logger.debug t.log
                    !"Adding %{sexp: User_command.With_valid_signature.t} to \
                      my pool locally, and scheduling for rebroadcast"
                    txn ;
                  (add' pool txn, (txn :> User_command.t) :: acc) ) )
      in
      t.pool <- pool' ;
      match res with
      | [] -> Deferred.Or_error.error_string "No new transactions"
      | xs -> Deferred.Or_error.return xs
  end

  (* TODO: Actually back this by the file-system *)
  let load ~disk_location:_ ~parent_log = return (create ~parent_log)
end
