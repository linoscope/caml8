open Caml8_lib
open Base
open Stdio

let%expect_test "Test Fishche.ch8" =
  let rom = In_channel.read_all "../resources/Fishie.ch8" |> Bytes.of_string in
  let cpu = Cpu.create ~rom in
  Cpu.tick cpu
