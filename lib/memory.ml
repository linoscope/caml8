open Base
open Ints

type t = bytes

let create () = Bytes.create 4096
let of_bytes b = b

let size t = Bytes.length t

let load t ~src ~dst_pos =
  Bytes.blito ~src ~dst:t ~dst_pos:(Uint16.to_int dst_pos) ()

let read_uint16 t ~pos = pos |> Uint16.to_int |> Caml.Bytes.get_int16_be t |> Uint16.of_int
let read_uint8 t ~pos = pos |> Uint16.to_int |> Caml.Bytes.get_int8 t |> Uint8.of_int
let write_uint16 t ~pos x = Caml.Bytes.set_int16_be t (Uint16.to_int pos) (Uint16.to_int x)
let write_uint8 t ~pos x = Caml.Bytes.set_int8 t (Uint16.to_int pos) (Uint8.to_int x)

let dump t ~pos ~len =
  let buf = Buffer.create 100 in
  for i = 0 to len - 1 do
    read_uint8 t ~pos:(pos + len + i * 2 |> Uint16.of_int)
    |> Uint8.to_int
    |> Printf.sprintf "%x\n"
    |> Buffer.add_string buf;
  done;
  Buffer.contents buf
