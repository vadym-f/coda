open Protocols
open Coda_pow
open Core_kernel
open Snark_pool
open Async

type ('work, 'priced_proof) diff =
  | Add_unsolved of 'work
  | Add_solved_work of 'work * 'priced_proof
[@@deriving bin_io]

module Make (Proof : sig
  type t [@@deriving bin_io]
end) (Fee : sig
  type t [@@deriving bin_io]
end) (Work : sig
  type t [@@deriving bin_io]
end)
(Pool : Snark_pool.S
        with type work := Work.t
         and type proof := Proof.t
         and type fee := Fee.t) =
struct
  type priced_proof = {proof: Proof.t; fee: Fee.t} [@@deriving bin_io]

  type t = (Work.t, priced_proof) diff [@@deriving bin_io]

  let apply (pool: Pool.t) (t: t) : t Or_error.t Deferred.t =
    let to_or_error = function
      | `Don't_rebroadcast ->
          Or_error.error_string "Worse fee or already in pool"
      | `Rebroadcast -> Ok t
    in
    ( match t with
    | Add_unsolved work -> Pool.add_unsolved_work pool work
    | Add_solved_work (work, {proof; fee}) ->
        Pool.add_snark pool ~work ~proof ~fee )
    |> to_or_error |> Deferred.return
end