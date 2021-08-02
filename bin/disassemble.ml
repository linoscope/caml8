open Base
open Stdio
open Caml8_lib

let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then begin
    prerr_endline "Usage : cemul <file name>";
    Caml.exit 2
  end;
  let in_ = In_channel.create argv.(1) in
  Disassembler.disassemble ~in_ ~out:Out_channel.stdout;
