open Base
open Stdint

type t = {
  memory : Memory.t;
  registers : Registers.t;
  mutable i : uint16;
  mutable pc : uint16;
}

let create () = {
  memory = Memory.create ();
  registers = Registers.create ();
  i = Uint16.zero;
  pc = Uint16.zero;
}

let add2 (pc : uint16) = Uint16.(pc + (of_int 2))

let tick t =
  match Instruction.read ~memory:t.memory ~pc:t.pc with
  | Ld_i_nnn data -> t.i <- data; t.pc <- add2 t.pc;
  | _ -> failwith "not implemented"
