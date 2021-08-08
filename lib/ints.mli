(* Implemented based on ocaml-integers.
 * Did not use ocaml-integers to avoid C bindings.  *)

module Uint8 : Ints_intf.S

module Uint16 : Ints_intf.S

type uint8 = Uint8.t

type uint16 = Uint16.t
