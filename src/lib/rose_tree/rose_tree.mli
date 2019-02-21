(**
 * A [Rose_tree.t] is a tree with at least 1 element where a node
 * has a variable number of successors.
 *
 * @see <https://en.wikipedia.org/wiki/Rose_tree> Wikipedia Article
 *)

open Core_kernel
open Async_kernel

type 'a t = T of 'a * 'a t list

val of_list_exn : 'a list -> 'a t

val iter : 'a t -> f:('a -> unit) -> unit

val fold_map : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b t

module Deferred : sig
  val iter : 'a t -> f:('a -> unit Deferred.t) -> unit Deferred.t

  val fold_map :
    'a t -> init:'b -> f:('b -> 'a -> 'b Deferred.t) -> 'b t Deferred.t

  module Or_error : sig
    val iter :
      'a t -> f:('a -> unit Deferred.Or_error.t) -> unit Deferred.Or_error.t

    val fold_map :
         'a t
      -> init:'b
      -> f:('b -> 'a -> 'b Deferred.Or_error.t)
      -> 'b t Deferred.Or_error.t
  end
end

module Or_error : sig
  val iter : 'a t -> f:('a -> unit Or_error.t) -> unit Or_error.t

  val fold_map :
    'a t -> init:'b -> f:('b -> 'a -> 'b Or_error.t) -> 'b t Or_error.t
end
