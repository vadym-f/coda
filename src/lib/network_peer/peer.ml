open Core

module T = struct
  type t =
    { (* the host is an IPv4 or IPv6 address *)
      host: Unix.Inet_addr.Blocking_sexp.t (* UDP *)
    ; discovery_port: int (* TCP *)
    ; communication_port: int }
  [@@deriving bin_io, sexp, compare, hash]
end

include T
include Hashable.Make (T)
include Comparable.Make_binable (T)

let create host ~discovery_port ~communication_port =
  {host; discovery_port; communication_port}

let to_string t = sexp_of_t t |> Sexp.to_string

let of_string s = Sexp.of_string s |> t_of_sexp

module Event = struct
  type nonrec t = Connect of t list | Disconnect of t list [@@deriving sexp]
end
