open Core
open Import
module Payload = User_command_payload

type ('payload, 'pk, 'signature) t_ =
  {payload: 'payload; sender: 'pk; signature: 'signature}
[@@deriving bin_io, eq, sexp, hash, yojson]

type t = (Payload.t, Public_key.t, Signature.t) t_
[@@deriving bin_io, eq, sexp, hash, yojson]

module Stable : sig
  module V1 : sig
    type nonrec ('payload, 'pk, 'signature) t_ =
                                                ('payload, 'pk, 'signature) t_ =
      {payload: 'payload; sender: 'pk; signature: 'signature}
    [@@deriving bin_io, eq, sexp, hash, yojson]

    type t = (Payload.Stable.V1.t, Public_key.t, Signature.t) t_
    [@@deriving bin_io, eq, sexp, hash, yojson]

    val compare : seed:string -> t -> t -> int
  end

  module Latest : module type of V1
end

val payload : t -> Payload.t

(* Generate a single transaction between
 * $a, b \in keys$
 * for fee $\in [0,max_fee]$
 * and an amount $\in [1,max_amount]$
 *)

val gen :
     keys:Signature_keypair.t array
  -> max_amount:int
  -> max_fee:int
  -> t Quickcheck.Generator.t

module With_valid_signature : sig
  module Stable : sig
    module V1 : sig
      type nonrec t = private t [@@deriving sexp, eq, bin_io]

      val compare : seed:string -> t -> t -> int

      val gen :
           keys:Signature_keypair.t array
        -> max_amount:int
        -> max_fee:int
        -> t Quickcheck.Generator.t
    end

    module Latest : module type of V1
  end

  include module type of Stable.Latest
end

val sign : Signature_keypair.t -> Payload.t -> With_valid_signature.t

val check : t -> With_valid_signature.t option

val accounts_accessed : t -> Public_key.Compressed.t list
