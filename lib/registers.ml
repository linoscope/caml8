open Base
open Ints

type register = int
let v0 = 0
let v1 = 1
let v2 = 2
let v3 = 3
let v4 = 4
let v5 = 5
let v6 = 6
let v7 = 7
let v8 = 8
let v9 = 9
let va = 10
let vb = 11
let vc = 12
let vd = 13
let ve = 14
let vf = 15
let register_of_int i = i
let register_of_int8 i = Uint8.to_int i
let register_to_string register = Printf.sprintf "V%x" register
let register_to_int register = register

type t = uint8 array
let create () = Array.create ~len:16 (Uint8.of_int 0)
let value t register = t.(register)
let set t register x = t.(register) <- x

let dump t =
  let buf = Buffer.create 100 in
  for i = 0 to 15 do
    let vx = register_of_int i in
    let s = Printf.sprintf "%s = 0x%x, " (vx |> register_to_string) (value t vx |> Uint8.to_int) in
    Buffer.add_string buf s
  done;
  Buffer.add_char buf '\n';
  Buffer.contents buf
