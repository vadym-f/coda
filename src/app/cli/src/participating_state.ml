open Core_kernel

module T = struct
  type 'a t = [`Active of 'a | `Bootstrapping]

  let return value = `Active value

  let bind x ~(f : 'a -> 'b t) =
    match x with `Active value -> f value | `Bootstrapping -> `Bootstrapping

  let map = `Define_using_bind
end

include T
include Monad.Make (T)

module Option = struct
  module T = struct
    type 'a t = 'a option T.t

    let return value = `Active (Some value)

    let bind value_option_status ~f =
      match value_option_status with
      | `Active (Some value_option) -> f value_option
      | `Active None -> `Active None
      | `Bootstrapping -> `Bootstrapping

    let map = `Define_using_bind
  end

  include Monad.Make (T)
end

let active_exn = function
  | `Active x -> x
  | `Bootstrapping -> failwith "Should be participating node"

let ignore = function `Active () -> () | `Bootstrapping -> ()

let rec sequence (list : 'a T.t List.t) : 'a List.t T.t =
  match list with
  | [] -> return []
  | [participating_state] ->
      bind participating_state ~f:(fun value -> return [value])
  | participating_state :: participating_states ->
      bind participating_state ~f:(fun x ->
          map (sequence participating_states) ~f:(fun sub_result ->
              x :: sub_result ) )
