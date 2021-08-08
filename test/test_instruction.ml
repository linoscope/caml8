open Base
open Stdio
open Caml8_lib.Ints
open Caml8_lib

let nnn = Uint16.of_int 0x1AB
let nn = Uint8.of_int 0x1A
let n = Uint8.of_int 0xA
let vx = Registers.v0
let vy = Registers.va
let instruction_examples = Instruction.[
    Cls;
    Ret;
    Ld_vx_dt vx;
    Jp_v0 nnn;
    Call nnn;
    Se_vx_nn (vx, nn);
    Se_vx_vy (vx, vy);
    Waitkey vx;
    Ld_i_nnn nnn;
    Spritechar vx;
    Regdump vx;
    Drw (vx, vy, n)
  ]

let%expect_test "test to_string" =
  instruction_examples
  |> List.map ~f:Instruction.to_string
  |> String.concat ~sep:"\n"
  |> print_endline;

  [%expect {|
    CLS
    RET
    LD         V0, DT
    JP         V0+0x1ab
    CALL       0x1ab
    SE         V0, 0x1a
    SE         V0, Va
    WAITKEY    V0
    LD         I, 0x1ab
    SPRITECHAR F, V0
    REGDUMP    V0~V0
    DRW        V0, Va, 0xa|}]
