(**
 * A [Rose_tree.t] is a tree with at least 1 element where a node
 * has a variable number of successors.
 *
 * @see <https://en.wikipedia.org/wiki/Rose_tree> Wikipedia Article
 *)

type 'a t = T of 'a * 'a t list

val of_list_exn : 'a list -> 'a t

val equal : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val equiv : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val iter : 'a t -> f:('a -> unit) -> unit

val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

val flatten : 'a t -> 'a list

module Deferred : sig
  open Async_kernel

  val iter : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t

  val fold_map :
    'a t -> init:'b -> f:('b -> 'a -> 'b Deferred.t) -> 'b t Deferred.t

  val all : 'a Deferred.t t -> 'a t Deferred.t
end
