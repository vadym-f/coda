open Core
open Snarky

module M = Snark.Make (Backends.Mnt4)
include M

module Hash = struct
    let hash = List.hd_exn
end

module Keys = Libsnark.Make_bg_ppzksnark_keys(Mnt4)(Hash)