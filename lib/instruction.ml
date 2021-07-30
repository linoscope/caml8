open Base
open Stdint

include Instruction_type

exception Unknown_instruction of string * uint16

let read ~memory ~pc =
  let uint16_to_nibbles (x : uint16) : int * int * int * int =
    let x = Uint16.to_int x in
    (x lsr 12, (0x0F00 land x) lsr 8, (0x00F0 land x) lsr 4, 0x000F land x)
  in
  let merge_nibbles3 n1 n2 n3 = (n1 lsl 8) + (n2 lsl 4) + n3 |> Uint16.of_int in
  let merge_nibbles2 n1 n2 = (n1 lsl 8) + n2 |> Uint8.of_int in
  let inst = Memory.read_uint16 memory pc in
  match inst |> uint16_to_nibbles with
  | (0x0, 0x0, 0xE, 0x0) -> Cls
  | (0x0, 0x0, 0xE, 0xE) -> Ret
  | (0x1, n1, n2, n3) -> Jp (merge_nibbles3 n1 n2 n3)
  | (0x2, n1, n2, n3) -> Call (merge_nibbles3 n1 n2 n3)
  | (0x3, x, n1, n2) -> Se_vx_nn (Registers.register_of_int x, merge_nibbles2 n1 n2)
  | (0x4, x, n1, n2) -> Sne_vx_nn (Registers.register_of_int x, merge_nibbles2 n1 n2)
  | (0x5, x, y, 0x0) -> Se_vx_vy (Registers.register_of_int x, Registers.register_of_int y)
  | (0x6, x, n1, n2) -> Ld_vx_nn (Registers.register_of_int x, merge_nibbles2 n1 n2)
  | (0x7, x, n1, n2) -> Add_vx_nn (Registers.register_of_int x, merge_nibbles2 n1 n2)
  | (0x8, x, y, 0x0) -> Ld_vx_vy (Registers.register_of_int x, Registers.register_of_int y)
  | (0x8, x, y, 0x1) -> Or (Registers.register_of_int x, Registers.register_of_int y)
  | (0x8, x, y, 0x2) -> And (Registers.register_of_int x, Registers.register_of_int y)
  | (0x8, x, y, 0x3) -> Xor (Registers.register_of_int x,  Registers.register_of_int y)
  | (0x8, x, y, 0x4) -> Add_vx_vy (Registers.register_of_int x, Registers.register_of_int y)
  | (0x8, x, _, 0x6) -> Shr (Registers.register_of_int x)
  | (0x8, x, y, 0x7) -> Subn (Registers.register_of_int x, Registers.register_of_int y)
  | (0x8, x, _, 0xE) -> Shl (Registers.register_of_int x)
  | (0x9, x, y, 0x0) -> Sne_vx_vy (Registers.register_of_int x, Registers.register_of_int y)
  | (0xA, n1, n2, n3) -> Ld_i_nnn (merge_nibbles3 n1 n2 n3)
  | (0xB, n1, n2, n3) -> Jp_v0 (merge_nibbles3 n1 n2 n3)
  | (0xC, x, n1, n2) -> Rnd (Registers.register_of_int x, merge_nibbles2 n1 n2)
  | (0xD, x, y, n1) -> Drw (Registers.register_of_int x, Registers.register_of_int y, Uint8.of_int n1)
  | (0xE, x, 0x9, 0xE) -> Skp (Registers.register_of_int x)
  | (0xE, x, 0xA, 0x1) -> Sknp (Registers.register_of_int x)
  | (0xF, x, 0x0, 0x7) -> Ld_vx_dt (Registers.register_of_int x)
  | (0xF, x, 0x0, 0xA) -> Waitkey (Registers.register_of_int x)
  | (0xF, x, 0x1, 0x5) -> Ld_dt_vx (Registers.register_of_int x)
  | (0xF, x, 0x1, 0x8) -> Ld_st_vx (Registers.register_of_int x)
  | (0xF, x, 0x1, 0xE) -> Add_i_vx (Registers.register_of_int x)
  | (0xF, x, 0x2, 0x9) -> Spritechar (Registers.register_of_int x)
  | (0xF, x, 0x3, 0x3) -> Bcd (Registers.register_of_int x)
  | (0xF, x, 0x5, 0x5) -> Ld_i_vx (Registers.register_of_int x)
  | (0xF, x, 0x6, 0x5) -> Ld_vx_i (Registers.register_of_int x)
  | _ ->
    let inst_str = inst |> Uint16.to_int |> Printf.sprintf "0x%04x" in
    raise (Unknown_instruction (inst_str, inst))

let to_string t =
  let op_vx_vy_to_string op vx vy =
    Printf.sprintf "%-10s %s, %s" op (Registers.register_to_string vx) (Registers.register_to_string vy)
  in
  let op_nnn_to_string op nnn =
    Printf.sprintf "%-10s 0x%x" op (Uint16.to_int nnn)
  in
  let op_vx_nn_to_string op vx nn =
    Printf.sprintf "%-10s %s, 0x%x" op (Registers.register_to_string vx) (Uint8.to_int nn)
  in
  let op_vx_to_string op vx =
    Printf.sprintf "%-10s %s" op (Registers.register_to_string vx)
  in
  match t with
  | Cls -> "CLS"
  | Ret -> "RET"
  | Jp nnn -> op_nnn_to_string "JP" nnn
  | Jp_v0 nnn -> Printf.sprintf "%-10s V0+0x%x" "JP" (Uint16.to_int nnn)
  | Call nnn -> op_nnn_to_string "CALL" nnn
  | Se_vx_nn (vx, nn) -> op_vx_nn_to_string "SE" vx nn
  | Se_vx_vy (vx, vy) -> op_vx_vy_to_string "SE" vx vy
  | Sne_vx_nn (vx, nn) -> op_vx_nn_to_string "SNE" vx nn
  | Sne_vx_vy (vx, vy) -> op_vx_vy_to_string "SNE" vx vy
  | Skp vx -> op_vx_to_string "SKP" vx
  | Sknp vx -> op_vx_to_string "SKNP" vx
  | Waitkey vx -> op_vx_to_string "WAITKEY" vx
  | Ld_vx_nn (vx, nn) -> op_vx_nn_to_string "LD" vx nn
  | Ld_vx_vy (vx, vy) ->  op_vx_vy_to_string "LD" vx vy
  | Ld_vx_dt vx -> Printf.sprintf "%-10s %s, DT" "LD" (Registers.register_to_string vx)
  | Ld_dt_vx vx -> Printf.sprintf "%-10s DT, %s" "LD" (Registers.register_to_string vx)
  | Ld_st_vx vx -> Printf.sprintf "%-10s ST, %s" "LD" (Registers.register_to_string vx)
  | Ld_i_nnn nnn -> Printf.sprintf "%-10s I, 0x%x" "LD" (Uint16.to_int nnn)
  | Spritechar vx -> Printf.sprintf "%-10s F, %s" "SPRITECHAR"  (Registers.register_to_string vx)
  | Ld_i_vx vx -> Printf.sprintf "%-10s I, %s" "LD" (Registers.register_to_string vx)
  | Ld_vx_i vx -> Printf.sprintf "%-10s %s, I" "LD" (Registers.register_to_string vx)
  | Add_i_vx vx -> Printf.sprintf "%-10s I, %s" "ADD" (Registers.register_to_string vx)
  | Add_vx_nn (vx, nn) -> op_vx_nn_to_string "ADD" vx nn
  | Add_vx_vy (vx, vy) -> op_vx_vy_to_string "ADD" vx vy
  | Sub (vx, vy) -> op_vx_vy_to_string "SUB" vx vy
  | Subn (vx, vy) -> op_vx_vy_to_string "SUBN" vx vy
  | Or (vx, vy) -> op_vx_vy_to_string "OR" vx vy
  | And (vx, vy) -> op_vx_vy_to_string "AND" vx vy
  | Xor (vx, vy) -> op_vx_vy_to_string "Xor" vx vy
  | Shr vx -> op_vx_to_string "SHR" vx
  | Shl vx -> op_vx_to_string "SHL" vx
  | Bcd vx -> op_vx_to_string "BCD" vx
  | Rnd (vx, nn) -> op_vx_nn_to_string "SHR" vx nn
  | Drw (vx, vy, n) ->
    Printf.sprintf "%-10s %s, %s, 0x%x" "DRW"
      (Registers.register_to_string vx) (Registers.register_to_string vy) (Uint8.to_int n)
