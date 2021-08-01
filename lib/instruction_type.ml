open Stdint

type t =
  | Cls
  | Ret
  | Jp of uint16
  | Jp_v0 of uint16
  | Call of uint16
  | Se_vx_nn of Registers.register * uint8
  | Se_vx_vy of Registers.register * Registers.register
  | Sne_vx_nn of Registers.register * uint8
  | Sne_vx_vy of Registers.register * Registers.register
  | Skp of Registers.register
  | Sknp of Registers.register
  | Waitkey of Registers.register
  | Ld_vx_nn of Registers.register * uint8
  | Ld_vx_vy of Registers.register * Registers.register
  | Ld_vx_dt of Registers.register
  | Ld_dt_vx of Registers.register
  | Ld_st_vx of Registers.register
  | Ld_i_nnn of uint16
  | Spritechar of Registers.register
  | Regdump of Registers.register
  | Regload of Registers.register
  | Add_i_vx of Registers.register
  | Add_vx_nn of Registers.register * uint8
  | Add_vx_vy of Registers.register * Registers.register
  | Sub of Registers.register * Registers.register
  | Subn of Registers.register * Registers.register
  | Or of Registers.register * Registers.register
  | And of Registers.register * Registers.register
  | Xor of Registers.register * Registers.register
  | Shr of Registers.register
  | Shl of Registers.register
  | Bcd of Registers.register
  | Rnd of Registers.register * uint8
  | Drw of Registers.register * Registers.register * uint8
