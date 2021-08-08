open Ints

type t

val create : unit -> t
val of_bytes : bytes -> t

val load : t -> src:bytes -> dst_pos:uint16 -> unit
val read_uint16 : t -> pos:uint16 -> uint16
val read_uint8 : t -> pos:uint16 -> uint8
val write_uint16 : t -> pos:uint16 -> uint16 -> unit
val write_uint8 : t -> pos:uint16 -> uint8 -> unit

val dump : t -> pos:int -> len:int -> string
