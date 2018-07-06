open Core
open Ctypes
open Foreign 

(*

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
type proof = unit ptr
let typ = ptr void



let prove =
    let stub =
    foreign "r1cs_bg_ppzksnark_split_prover"
    (bool list @-> pk @-> aux @-> witness @-> proof @->
    returning proof)


(* we need to cycle through the bits not just 0 *)
let get_bit x =
    test_bit (of_field (x)) 0))





