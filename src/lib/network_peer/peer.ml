open Core

module T = struct
  type t =
    { (* the host is an IPv4 or IPv6 address *)
      host: Unix.Inet_addr.Blocking_sexp.t
    ; kademlia_port: int
    ; rpc_port: int }
  [@@deriving bin_io, sexp, compare, hash]
end

include T
include Hashable.Make (T)

let external_rpc t =
  let host = Unix.Inet_addr.to_string t.host in
  let port = t.rpc_port in
  Host_and_port.create ~host ~port

let create host ~kademlia_port ~rpc_port = {host; kademlia_port; rpc_port}

module Event = struct
  type nonrec t = Connect of t list | Disconnect of t list [@@deriving sexp]
end
