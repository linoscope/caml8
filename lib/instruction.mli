open Stdint

include module type of Instruction_type

exception Unknown_instruction of string * uint16

val read : memory:Memory.t -> pc:uint16 -> t
val to_string : t -> string
