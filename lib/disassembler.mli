open Stdio

(** [dissasemble ~in_ out] decodes instructions from [~in_] and outputs the string representation of the instructions to [~out].  *)
val disassemble : in_:In_channel.t -> out:Out_channel.t -> unit
