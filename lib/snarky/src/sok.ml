open Core
open Ctypes
open Foreign 

(*

can we open libsnark.ml? :(

val sok_sign
    :  message:bool list
    -> proving_key
    -> input
    -> witness
    -> proof

hash function:
- have c++ bindings to pull A, B, C, delta_prime out of the proof
    - have bindings for G1 points
        - pull out x
        - pull out y

    - have bindings for G2 points
        - have type base_field
        - pull out x : base_field
            - have a binding to a function
                base_field -> Field.t vector
                    (put method in libff)

- you are done when you have '''proof'''_to_bits function
*)

(* we should have this actually defined properly rather than 
just be a pointer / expecting void*)

(*
type proof = unit ptr
let typ = ptr void

let prove =
    let stub =
    foreign "r1cs_bg_ppzksnark_split_prover"
    

(* we need to cycle through the bits not just the 0th *)
let get_bit_list x =
    test_bit (of_field (x)) 0))


*)


