(** Any_ledger lets you use any arbitrary ledger whenever some ledger is
 * required. This uses dynamic dispatch and is equivalent to the notion of
 * consuming a value conforming to an interface in Java.
 *
 * It uses GADTs to type-erase the specific underlying first-class module
 * for some given signature and delegates all function calls.
 *
 * The restriction here is that one cannot conform to some signature that
 * exposes a `create` function because we can't magically pull a conforming
 * module out of thin air. However, one can always just pack any concrete
 * instance with the GADT constructor `witness`.
 *
 * Props to @nholland for showing me this trick.
 * *)

open Core_kernel

module type S = sig
  type key

  type key_set

  type account

  type hash

  module Location : Location_intf.S

  (** The type of the witness for a base ledger exposed here so that it can
   * be easily accessed from outside this module *)
  type witness [@@deriving sexp_of]

  module type Base_intf =
    Base_ledger_intf.S
    with module Addr = Location.Addr
    with module Location = Location
    with type key := key
     and type key_set := key_set
     and type hash := hash
     and type root_hash := hash
     and type account := account

  val cast : (module Base_intf with type t = 'a) -> 'a -> witness

  module M : Base_intf with type t = witness
end

module Make_base
    (Key : Intf.Key)
    (Account : Intf.Account with type key := Key.t)
    (Hash : Intf.Hash with type account := Account.t)
    (Location : Location_intf.S) (Depth : sig
        val depth : int
    end) :
  S
  with module Location = Location
  with type key := Key.t
   and type hash := Hash.t
   and type key_set := Key.Set.t
   and type account := Account.t = struct
  module Location = Location

  module type Base_intf =
    Base_ledger_intf.S
    with module Addr = Location.Addr
    with module Location = Location
    with type key := Key.t
     and type key_set := Key.Set.t
     and type hash := Hash.t
     and type root_hash := Hash.t
     and type account := Account.t

  type witness = T : (module Base_intf with type t = 't) * 't -> witness

  let cast (m : (module Base_intf with type t = 'a)) (t : 'a) = T (m, t)

  let sexp_of_witness (T ((module B), t)) = B.sexp_of_t t

  (** M can be used wherever a base ledger is demanded, construct instances
   * by using the witness constructor directly
   *
   * We delegate to the underlying functions in the base interface mechanically
   *
   * In the future, this should be a `ppx`.
   *)
  module M : Base_intf with type t = witness = struct
    type t = witness [@@deriving sexp_of]

    let t_of_sexp _ = failwith "t_of_sexp unimplemented"

    type index = int

    module Location = Location
    module Path = Merkle_path.Make (Hash)

    type path = Path.t

    module Addr = Location.Addr

    let remove_accounts_exn (T ((module Base), t)) = Base.remove_accounts_exn t

    let merkle_path_at_index_exn (T ((module Base), t)) =
      Base.merkle_path_at_index_exn t

    let merkle_path (T ((module Base), t)) = Base.merkle_path t

    let merkle_root (T ((module Base), t)) = Base.merkle_root t

    let index_of_key_exn (T ((module Base), t)) = Base.index_of_key_exn t

    let set_at_index_exn (T ((module Base), t)) = Base.set_at_index_exn t

    let get_at_index_exn (T ((module Base), t)) = Base.get_at_index_exn t

    let set_batch (T ((module Base), t)) = Base.set_batch t

    let set (T ((module Base), t)) = Base.set t

    let get (T ((module Base), t)) = Base.get t

    let get_uuid (T ((module Base), t)) = Base.get_uuid t

    let last_filled (T ((module Base), t)) = Base.last_filled t

    let close (T ((module Base), t)) = Base.close t

    let get_or_create_account_exn (T ((module Base), t)) =
      Base.get_or_create_account_exn t

    let get_or_create_account (T ((module Base), t)) =
      Base.get_or_create_account t

    let location_of_key (T ((module Base), t)) = Base.location_of_key t

    let fold_until (T ((module Base), t)) = Base.fold_until t

    let keys (T ((module Base), t)) = Base.keys t

    let iteri (T ((module Base), t)) = Base.iteri t

    (* ignored_keys must be Base.Keys.Set.t, but that isn't necessarily the same as Keys.Set.t for the
       Keys passed to this functor; as long as we use the same Keys for all ledgers, this should work
     *)
    let foldi_with_ignored_keys (T ((module Base), t)) =
      Base.foldi_with_ignored_keys t

    let foldi (T ((module Base), t)) = Base.foldi t

    let to_list (T ((module Base), t)) = Base.to_list t

    let make_space_for (T ((module Base), t)) = Base.make_space_for t

    let get_all_accounts_rooted_at_exn (T ((module Base), t)) =
      Base.get_all_accounts_rooted_at_exn t

    let set_all_accounts_rooted_at_exn (T ((module Base), t)) =
      Base.set_all_accounts_rooted_at_exn t

    let set_batch_accounts (T ((module Base), t)) = Base.set_batch_accounts t

    let set_inner_hash_at_addr_exn (T ((module Base), t)) =
      Base.set_inner_hash_at_addr_exn t

    let get_inner_hash_at_addr_exn (T ((module Base), t)) =
      Base.get_inner_hash_at_addr_exn t

    let merkle_path_at_addr_exn (T ((module Base), t)) =
      Base.merkle_path_at_addr_exn t

    let num_accounts (T ((module Base), t)) = Base.num_accounts t

    (* This better be the same depth inside Base or you're going to have a bad
     * time *)
    let depth = Depth.depth
  end
end
