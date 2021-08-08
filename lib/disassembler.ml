open Base
open Stdio
open Ints

let disassemble ~in_ ~out =
  let size = In_channel.length in_ |> Int64.to_int_exn in
  let buf = Bytes.create (0x200 + size) in
  In_channel.really_input_exn in_ ~buf ~pos:0x200 ~len:size;
  let memory = Memory.of_bytes buf in
  let pc = ref (0x200 |> Uint16.of_int) in
  while (Uint16.to_int !pc < 0x200 + size) do
    Out_channel.output_string out (Printf.sprintf "0x%04x " (Uint16.to_int !pc));
    begin try
        Instruction.read ~memory:memory ~pc:!pc
        |> Instruction.to_string
        |> Out_channel.output_string out;
      with
        Instruction.Unknown_instruction (_, inst) ->
        Out_channel.output_string out (Printf.sprintf "%-10s 0x%04x" "DATA" (Uint16.to_int inst))
    end;
    Out_channel.newline out;
    pc := Uint16.(!pc + of_int 2)
  done;
