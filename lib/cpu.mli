type t

val create : rom:bytes -> t

val tick : t -> unit

val get_gfx : t -> bool array
