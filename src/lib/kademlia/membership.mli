open Async_kernel
open Pipe_lib
open Network_peer

exception Child_died

module Haskell : sig
  type t

  val connect :
       initial_peers:Discovery_peer.t list
    -> me:Peer.t
    -> parent_log:Logger.t
    -> conf_dir:string
    -> banlist:Coda_base.Banlist.t
    -> t Deferred.Or_error.t

  val peers : t -> Discovery_peer.t list

  val first_peers : t -> Discovery_peer.t list Deferred.t

  val changes : t -> Discovery_peer.Event.t Linear_pipe.Reader.t

  val stop : t -> unit Deferred.t
end
