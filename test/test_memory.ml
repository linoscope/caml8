open Stdio
open Base
open Caml8_lib
open Caml8_lib.Ints

let%expect_test "test" =
  let memory = Memory.create () in
  Memory.write_uint16 memory ~pos:(10 |> Uint16.of_int) (0xabcd |> Uint16.of_int);

  Memory.read_uint16 memory ~pos:(10 |> Uint16.of_int)
  |> Uint16.to_int
  |> printf "%x";

  [%expect {| abcd |}];

  Memory.read_uint8 memory ~pos:(10 |> Uint16.of_int)
  |> Uint8.to_int
  |> printf "%x";

  [%expect {| ab |}];

  Memory.size memory
  |> printf "%x";

  [%expect {| 1000 |}];
