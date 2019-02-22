[%%import
"../../../config.mlh"]

open Core
open Async
open Coda_base
open Util
open Blockchain_snark
open Cli_lib
open Snark_params

module type S = sig
  type t

  val create : conf_dir:string -> t Deferred.t

  val verify_blockchain : t -> Blockchain.t -> bool Or_error.t Deferred.t

  val verify_transaction_snark :
       t
    -> Transaction_snark.t
    -> message:Sok_message.t
    -> bool Or_error.t Deferred.t
end

module Worker_state = struct
  module type S = sig
    val verify_wrap : Consensus.Protocol_state.value -> Tock.Proof.t -> bool

    val verify_transaction_snark :
      Transaction_snark.t -> message:Sok_message.t -> bool
  end

  type init_arg = unit [@@deriving bin_io]

  type t = (module S) Deferred.t

  let create () : t Deferred.t =
    Deferred.return
      (let%map bc_vk = Snark_keys.blockchain_verification ()
       and tx_vk = Snark_keys.transaction_verification () in
       let module T = Transaction_snark.Verification.Make (struct
         let keys = tx_vk
       end) in
       let module B = Blockchain_transition.Make (Consensus) (T) in
       let module U =
         Blockchain_snark_utils.Verification
           (Consensus)
           (struct
             let key = bc_vk.wrap

             let key_to_bool_list = Snark_params.tock_vk_to_bool_list
           end)
       in
       let module M = struct
         let verify_wrap = U.verify_wrap

         let verify_transaction_snark = T.verify
       end in
       (module M : S))

  let get = Fn.id
end

module Worker = struct
  module T = struct
    module F = Rpc_parallel.Function

    type 'w functions =
      { verify_blockchain: ('w, Blockchain.t, bool) F.t
      ; verify_transaction_snark:
          ('w, Transaction_snark.t * Sok_message.t, bool) F.t }

    module Worker_state = Worker_state

    module Connection_state = struct
      type init_arg = unit [@@deriving bin_io]

      type t = unit
    end

    module Functions
        (C : Rpc_parallel.Creator
             with type worker_state := Worker_state.t
              and type connection_state := Connection_state.t) =
    struct
      let verify_blockchain (w : Worker_state.t) (chain : Blockchain.t) =
        match Coda_compile_config.proof_level with
        | "full" ->
            let%map (module M) = Worker_state.get w in
            M.verify_wrap chain.state chain.proof
        | "check" | "none" -> Deferred.return true
        | _ -> failwith "unknown proof_level"

      let verify_transaction_snark (w : Worker_state.t) (p, message) =
        match Coda_compile_config.proof_level with
        | "full" ->
            let%map (module M) = Worker_state.get w in
            M.verify_transaction_snark p ~message
        | "check" | "none" -> Deferred.return true
        | _ -> failwith "unknown proof_level"

      let functions =
        let f (i, o, f) =
          C.create_rpc
            ~f:(fun ~worker_state ~conn_state i -> f worker_state i)
            ~bin_input:i ~bin_output:o ()
        in
        { verify_blockchain= f (Blockchain.bin_t, Bool.bin_t, verify_blockchain)
        ; verify_transaction_snark=
            f
              ( [%bin_type_class: Transaction_snark.t * Sok_message.t]
              , Bool.bin_t
              , verify_transaction_snark ) }

      let init_worker_state () = Worker_state.create ()

      let init_connection_state ~connection:_ ~worker_state:_ = return
    end
  end

  include Rpc_parallel.Make (T)
end

type t = Worker.Connection.t

let create ~conf_dir =
  let%map connection, process =
    Worker.spawn_in_foreground_exn ~connection_timeout:(Time.Span.of_min 1.)
      ~on_failure:Error.raise ~shutdown_on:Disconnect
      ~connection_state_init_arg:() ()
  in
  File_system.dup_stdout process ;
  File_system.dup_stderr process ;
  connection

let verify_blockchain t chain =
  Worker.Connection.run t ~f:Worker.functions.verify_blockchain ~arg:chain

let verify_transaction_snark t snark ~message =
  Worker.Connection.run t ~f:Worker.functions.verify_transaction_snark
    ~arg:(snark, message)
