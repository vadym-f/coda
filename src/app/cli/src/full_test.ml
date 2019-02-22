[%%import
"../../../config.mlh"]

open Core
open Async
open Coda_base
open Coda_main
open Signature_lib
open Pipe_lib
open O1trace

let pk_of_sk sk = Public_key.of_private_key_exn sk |> Public_key.compress

[%%if
proof_level = "full"]

let with_snark = true

[%%else]

let with_snark = false

[%%endif]

let run_test () : unit Deferred.t =
  Parallel.init_master () ;
  let log = Logger.create () in
  File_system.with_temp_dir "full_test_config" ~f:(fun temp_conf_dir ->
      let keypair = Genesis_ledger.largest_account_keypair_exn () in
      let module Config = struct
        let logger = log

        let conf_dir = temp_conf_dir

        let lbc_tree_max_depth = `Finite 50

        let propose_keypair = Some keypair

        let genesis_proof = Precomputed_values.base_proof

        let commit_id = None

        let work_selection = Protocols.Coda_pow.Work_selection.Seq
      end in
      let%bind (module Init) =
        make_init ~should_propose:true (module Config)
      in
      let%bind () =
        if Unix.getenv "CODA_TRACING" = Some "1" then
          let%bind () = Async.Unix.mkdir ~p:() "/tmp/full-test-traces" in
          Coda_tracing.start "/tmp/full-test-traces"
        else Deferred.unit
      in
      let module Main = Coda_main.Make_coda (Init) in
      let module Run = Run (Config) (Main) in
      let open Main in
      let banlist_dir_name = temp_conf_dir ^/ "banlist" in
      let%bind () = Async.Unix.mkdir banlist_dir_name in
      let%bind suspicious_dir =
        Async.Unix.mkdtemp (banlist_dir_name ^/ "suspicious")
      in
      let%bind punished_dir =
        Async.Unix.mkdtemp (banlist_dir_name ^/ "banned")
      in
      let banlist = Coda_base.Banlist.create ~suspicious_dir ~punished_dir in
      let%bind trust_dir = Async.Unix.mkdtemp (temp_conf_dir ^/ "trust_db") in
      let trust_system = Coda_base.Trust_system.create ~db_dir:trust_dir in
      let%bind receipt_chain_dir_name =
        Async.Unix.mkdtemp (temp_conf_dir ^/ "receipt_chain")
      in
      let receipt_chain_database =
        Coda_base.Receipt_chain_database.create
          ~directory:receipt_chain_dir_name
      in
      let time_controller = Inputs.Time.Controller.create () in
      let net_config =
        { Inputs.Net.Config.parent_log= log
        ; time_controller
        ; gossip_net_params=
            { Inputs.Net.Gossip_net.Config.timeout= Time.Span.of_sec 1.
            ; parent_log= log
            ; target_peer_count= 8
            ; initial_peers= []
            ; conf_dir= temp_conf_dir
            ; me=
                Network_peer.Peer.create Unix.Inet_addr.localhost
                  ~discovery_port:8001 ~communication_port:8000
            ; trust_system } }
      in
      Core.Backtrace.elide := false ;
      Async.Scheduler.set_record_backtraces true ;
      let%bind coda =
        Main.create
          (Main.Config.make ~log ~net_config ~propose_keypair:keypair
             ~run_snark_worker:true
             ~staged_ledger_persistant_location:
               (temp_conf_dir ^/ "staged_ledger")
             ~transaction_pool_disk_location:
               (temp_conf_dir ^/ "transaction_pool")
             ~snark_pool_disk_location:(temp_conf_dir ^/ "snark_pool")
             ~time_controller ~receipt_chain_database () ~banlist
             ~snark_work_fee:(Currency.Fee.of_int 0))
      in
      Main.start coda ;
      don't_wait_for
        (Strict_pipe.Reader.iter_without_pushback
           (Main.strongest_ledgers coda)
           ~f:ignore) ;
      let wait_until_cond ~(f : t -> bool) ~(timeout : Float.t) =
        let rec go () =
          if f coda then return ()
          else
            let%bind () = after (Time.Span.of_sec 10.) in
            go ()
        in
        Deferred.any [after (Time.Span.of_min timeout); go ()]
      in
      let balance_change_or_timeout ~initial_receiver_balance receiver_pk =
        let cond t =
          match
            Run.get_balance t receiver_pk |> Participating_state.active_exn
          with
          | Some b when not (Currency.Balance.equal b initial_receiver_balance)
            ->
              true
          | _ -> false
        in
        wait_until_cond ~f:cond ~timeout:3.
      in
      let assert_balance pk amount =
        match Run.get_balance coda pk |> Participating_state.active_exn with
        | Some balance ->
            if not (Currency.Balance.equal balance amount) then
              failwithf
                !"Balance in account (%{sexp: Public_key.Compressed.t}) \
                  %{sexp: Currency.Balance.t} is not asserted balance %{sexp: \
                  Currency.Balance.t}"
                pk balance amount ()
        | None ->
            failwith
              (sprintf !"Invalid Account: %{sexp: Public_key.Compressed.t}" pk)
      in
      let client_port = 8123 in
      let largest_account_keypair =
        Genesis_ledger.largest_account_keypair_exn ()
      in
      let run_snark_worker =
        `With_public_key
          (Public_key.compress largest_account_keypair.public_key)
      in
      Run.setup_local_server ~client_port ~coda ~log () ;
      Run.run_snark_worker ~log ~client_port run_snark_worker ;
      (* Let the system settle *)
      let%bind () = Async.after (Time.Span.of_ms 100.) in
      (* No proof emitted by the parallel scan at the begining *)
      assert (Option.is_none @@ Run.For_tests.ledger_proof coda) ;
      (* Note: This is much less than half of the high balance account so we can test
   *       payment replays being prohibited
   *)
      let send_amount = Currency.Amount.of_int 10 in
      (* Send money to someone *)
      let build_payment amount sender_sk receiver_pk fee =
        trace_recurring_task "build_payment" (fun () ->
            let nonce =
              Run.get_nonce coda (pk_of_sk sender_sk)
              |> Participating_state.active_exn |> Option.value_exn
            in
            let payload : User_command.Payload.t =
              User_command.Payload.create ~fee ~nonce
                ~memo:User_command_memo.dummy
                ~body:(Payment {receiver= receiver_pk; amount})
            in
            User_command.sign (Keypair.of_private_key_exn sender_sk) payload )
      in
      let assert_ok x = assert (Or_error.is_ok x) in
      let test_sending_payment sender_sk receiver_pk =
        let payment =
          build_payment send_amount sender_sk receiver_pk
            (Currency.Fee.of_int 0)
        in
        let prev_sender_balance =
          Run.get_balance coda (pk_of_sk sender_sk)
          |> Participating_state.active_exn |> Option.value_exn
        in
        let prev_receiver_balance =
          Run.get_balance coda receiver_pk
          |> Participating_state.active_exn
          |> Option.value ~default:Currency.Balance.zero
        in
        let%bind p1_res =
          Run.send_payment log coda (payment :> User_command.t)
        in
        assert_ok (p1_res |> Participating_state.active_exn) ;
        (* Send a similar payment twice on purpose; this second one will be rejected
       because the nonce is wrong *)
        let payment' =
          build_payment send_amount sender_sk receiver_pk
            (Currency.Fee.of_int 0)
        in
        let%bind p2_res =
          Run.send_payment log coda (payment' :> User_command.t)
        in
        assert_ok (p2_res |> Participating_state.active_exn) ;
        (* The payment fails, but the rpc command doesn't indicate that because that
       failure comes from the network. *)
        (* Let the system settle, mine some blocks *)
        let%map () =
          balance_change_or_timeout
            ~initial_receiver_balance:prev_receiver_balance receiver_pk
        in
        assert_balance receiver_pk
          ( Currency.Balance.( + ) prev_receiver_balance send_amount
          |> Option.value_exn ) ;
        assert_balance (pk_of_sk sender_sk)
          ( Currency.Balance.( - ) prev_sender_balance send_amount
          |> Option.value_exn )
      in
      let send_payment_update_balance_sheet sender_sk sender_pk receiver_pk
          amount balance_sheet fee =
        let payment = build_payment amount sender_sk receiver_pk fee in
        let new_balance_sheet =
          Map.update balance_sheet sender_pk (fun v ->
              Option.value_exn
                (Currency.Balance.sub_amount (Option.value_exn v)
                   (Option.value_exn (Currency.Amount.add_fee amount fee))) )
        in
        let new_balance_sheet' =
          Map.update new_balance_sheet receiver_pk (fun v ->
              Option.value_exn
                (Currency.Balance.add_amount (Option.value_exn v) amount) )
        in
        let%map p_res =
          Run.send_payment log coda (payment :> User_command.t)
        in
        p_res |> Participating_state.active_exn |> assert_ok ;
        new_balance_sheet'
      in
      let send_payments accounts pks balance_sheet f_amount =
        Deferred.List.foldi accounts ~init:balance_sheet
          ~f:(fun i acc ((keypair : Signature_lib.Keypair.t), _) ->
            let sender_pk = Public_key.compress keypair.public_key in
            let receiver =
              List.random_element_exn
                (List.filter pks ~f:(fun pk -> not (pk = sender_pk)))
            in
            send_payment_update_balance_sheet keypair.private_key sender_pk
              receiver (f_amount i) acc (Currency.Fee.of_int 0) )
      in
      let block_count t =
        Run.best_protocol_state t |> Participating_state.active_exn
        |> Inputs.Consensus_mechanism.Protocol_state.consensus_state
        |> Inputs.Consensus_mechanism.Consensus_state.length
      in
      let wait_for_proof_or_timeout timeout () =
        let cond t = Option.is_some @@ Run.For_tests.ledger_proof t in
        wait_until_cond ~f:cond ~timeout
      in
      let test_multiple_payments accounts pks timeout =
        let balance_sheet =
          Public_key.Compressed.Map.of_alist_exn
            (List.map accounts
               ~f:(fun ((keypair : Signature_lib.Keypair.t), account) ->
                 ( Public_key.compress keypair.public_key
                 , Account.balance account ) ))
        in
        let%bind updated_balance_sheet =
          send_payments accounts pks balance_sheet (fun i ->
              Currency.Amount.of_int ((i + 1) * 10) )
        in
        (*After mining a few blocks and emitting a ledger_proof (by the parallel scan), check if the balances match *)
        let%map () = wait_for_proof_or_timeout timeout () in
        assert (Option.is_some @@ Run.For_tests.ledger_proof coda) ;
        Map.fold updated_balance_sheet ~init:() ~f:(fun ~key ~data () ->
            assert_balance key data ) ;
        block_count coda
      in
      let test_duplicate_payments (sender_keypair : Signature_lib.Keypair.t)
          (receiver_keypair : Signature_lib.Keypair.t) =
        let%bind () =
          test_sending_payment sender_keypair.private_key
            (Public_key.compress receiver_keypair.public_key)
        in
        test_sending_payment sender_keypair.private_key
          (Public_key.compress receiver_keypair.public_key)
      in
      let pks accounts =
        List.map accounts ~f:(fun ((keypair : Signature_lib.Keypair.t), _) ->
            Public_key.compress keypair.public_key )
      in
      (*Need some accounts from the genesis ledger to test payment replays and
  sending multiple payments*)
      let receiver_keypair =
        let receiver =
          Genesis_ledger.find_new_account_record_exn
            [largest_account_keypair.public_key]
        in
        Genesis_ledger.keypair_of_account_record_exn receiver
      in
      let sender_keypair =
        let sender =
          Genesis_ledger.find_new_account_record_exn
            [largest_account_keypair.public_key; receiver_keypair.public_key]
        in
        Genesis_ledger.keypair_of_account_record_exn sender
      in
      let other_accounts =
        List.filter Genesis_ledger.accounts ~f:(fun (_, account) ->
            let reserved_public_keys =
              [ largest_account_keypair.public_key
              ; receiver_keypair.public_key
              ; sender_keypair.public_key ]
            in
            not
              (List.exists reserved_public_keys ~f:(fun pk ->
                   Public_key.equal pk
                     (Public_key.decompress_exn @@ Account.public_key account)
               )) )
        |> List.map ~f:(fun (sk, account) ->
               ( Genesis_ledger.keypair_of_account_record_exn (sk, account)
               , account ) )
      in
      if with_snark then
        let accounts = List.take other_accounts 2 in
        let%bind block_count' =
          test_multiple_payments accounts (pks accounts) 15.
        in
        (*wait for a block after the ledger_proof is emitted*)
        let%map () =
          wait_until_cond
            ~f:(fun t -> block_count t > block_count')
            ~timeout:5.
        in
        assert (block_count coda > block_count')
      else
        let%bind _ =
          test_multiple_payments other_accounts (pks other_accounts) 5.
        in
        test_duplicate_payments sender_keypair receiver_keypair )

let command =
  let open Core in
  let open Async in
  Command.async ~summary:"Full coda end-to-end test"
    (Command.Param.return run_test)
