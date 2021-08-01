open Caml8_lib
open Base
open Stdio
open Tsdl

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; Caml.exit 1
  | Ok x -> x

let init_graphics () =
  Sdl.init Sdl.Init.video |> or_exit;
  let w = Sdl.create_window ~w:640 ~h:320 "SDL OpenGL" Sdl.Window.opengl |> or_exit in
  Sdl.create_renderer w ~index:(-1) |> or_exit

let () =
  let rom = In_channel.read_all "resources/test_opcode.ch8" |> Bytes.of_string in
  let cpu = Cpu.create ~rom in
  let renderer = init_graphics () in
  let clear () =
    Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
    Sdl.render_clear renderer |> or_exit
  in
  clear ();
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  while true do
    Cpu.tick cpu;
    let gfx = Cpu.get_gfx cpu in
    gfx |> Array.iteri ~f:(fun i b ->
        if b then begin
          let rect = Sdl.Rect.create ~x:(10 * (i % 64)) ~y:(10 * (i / 64)) ~w:10 ~h:10 in
          Sdl.render_fill_rect renderer (Some rect) |> or_exit;
        end
      );
    Sdl.render_present renderer;
  done
