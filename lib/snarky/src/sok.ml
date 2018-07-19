open Core
open Ctypes
open Foreign 

(*

- [ ] val sok_sign
    :  message:bool list
    -> proving_key
    -> input
    -> witness
    -> proof

- [x] hash function:
- [x] - have c++ bindings to pull A, B, C, delta_prime out of the proof
    - [x] - have bindings for G1 points
    - [x] - pull out x
    - [x] - pull out y

 - [x] - have bindings for G2 points
    - [x] - pull out x : base_field
        - [ ] - have a binding to a function
                base_field -> Field.t vector
                    (put method in libff)

- [x] - you are done when you have '''proof'''_to_bits function
*)



