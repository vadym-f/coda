open Core
open Network_peer

module Incoming = struct
  type 'a t = {data: 'a; sender: Peer.t; ephemeral_port: int}

  let sender {sender; _} = sender

  let data {data; _} = data

  let ephemeral_port {ephemeral_port; _} = ephemeral_port

  let wrap ~data ~sender ~ephemeral_port = {data; sender; ephemeral_port}

  let map ~f t = {t with data= f t.data}

  let local data =
    { data
    ; sender= Peer.create Unix.Inet_addr.localhost ~kademlia_port:0 ~rpc_port:0
    ; ephemeral_port= 0 }
end
