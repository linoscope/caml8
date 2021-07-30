open Base
open Stdint

type t = bytes

let create () = Bytes.create 4096
let of_bytes b = b

let read_uint16 t i = i |> Uint16.to_int |> Caml.Bytes.get_int16_be t |> Uint16.of_int

let read_uint8 t i = i |> Uint16.to_int |> Caml.Bytes.get_int8 t |> Uint8.of_int
