open Base
open Stdio
open Ints

(** [build_padded_memory channel size]
    Build Memory to store the backing input channel channel,
    padding out to an even byte-boundary with zeros *)
let build_padded_memory in_channel =
  let size = In_channel.length in_channel |> Int64.to_int_exn in
  let padded_size = if size % 2 = 0 then size else Int.succ size in
  let buf = (Bytes.create (0x200 + padded_size)) in
  (* Set the last byte to zero if this is an odd-length file *)
  if not (size % 2 = 0) then Bytes.set buf size (Char.of_int_exn 0);
  In_channel.really_input_exn in_channel ~buf ~pos:0x200 ~len:size;
  Memory.of_bytes buf

let disassemble ~in_ ~out =
  let memory = build_padded_memory in_ in
  let size = Memory.size memory  in
  let pc = ref (0x200 |> Uint16.of_int) in
  while (Uint16.to_int !pc < size) do
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
