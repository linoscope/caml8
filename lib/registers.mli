open Stdint

type register
val v0 : register
val v1 : register
val v2 : register
val v3 : register
val v4 : register
val v5 : register
val v6 : register
val v7 : register
val v8 : register
val v9 : register
val va : register
val vb : register
val vc : register
val vd : register
val ve : register
val vf : register
val register_of_int : int -> register
val register_of_int8 : uint8 -> register
val register_to_string : register -> string

type t
val create : unit -> t
val value : t -> register -> uint8
val set : t -> register -> uint8 -> unit
val dump : t -> string
