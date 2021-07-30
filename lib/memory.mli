open Stdint

type t

val create : unit -> t
val of_bytes : bytes -> t

val read_uint16 : t -> uint16 -> uint16
val read_uint8 : t -> uint16 -> uint8
