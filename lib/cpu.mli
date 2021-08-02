open Stdint

type t

val create : rom:bytes -> t

val tick : t -> unit

val get_gfx : t -> bool array

val set_key : t -> uint8 option -> unit
