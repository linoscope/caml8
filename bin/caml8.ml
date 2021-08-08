open Caml8_lib
open Base
open Ints
open Stdio
open Tsdl

let last_tick = ref 0.

let or_exit = function
  | Error (`Msg e) -> Sdl.log "%s" e; Caml.exit 1
  | Ok x -> x

let init_graphics () =
  Sdl.init Sdl.Init.video |> or_exit;
  let w = Sdl.create_window ~w:640 ~h:320 "Caml8" Sdl.Window.opengl |> or_exit in
  let renderer = Sdl.create_renderer w ~index:(-1) |> or_exit in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  renderer


let clear_graphics renderer =
  Sdl.set_render_draw_color renderer 0x00 0x00 0x00 0xFF |> or_exit;
  Sdl.render_clear renderer |> or_exit

let draw_graphics cpu renderer =
  let gfx = Cpu.get_gfx cpu in
  Sdl.set_render_draw_color renderer 0xFF 0xFF 0xFF 0xFF |> or_exit;
  gfx |> Array.iteri ~f:(fun i b ->
      if b then begin
        let rect = Sdl.Rect.create ~x:(10 * (i % 64)) ~y:(10 * (i / 64)) ~w:10 ~h:10 in
        Sdl.render_fill_rect renderer (Some rect) |> or_exit;
      end
    );
  Sdl.render_present renderer

let handle_event cpu =
  let set_some_key cpu x = Cpu.set_key cpu (Some (Uint8.of_int x)) in
  let set_none_key cpu = Cpu.set_key cpu None in
  let event = Sdl.Event.create () in
  if Sdl.poll_event (Some event) then begin
    match Sdl.Event.(get event typ |> enum) with
    | `Key_down ->
      let scancode = Sdl.Event.(get event keyboard_scancode) in
      begin match Sdl.Scancode.enum scancode with
        | `K1     -> set_some_key cpu 0x1
        | `K2     -> set_some_key cpu 0x2
        | `K3     -> set_some_key cpu 0x3
        | `K4     -> set_some_key cpu 0xC
        | `Q      -> set_some_key cpu 0x4
        | `W      -> set_some_key cpu 0x5
        | `E      -> set_some_key cpu 0x6
        | `R      -> set_some_key cpu 0xD
        | `A      -> set_some_key cpu 0x7
        | `S      -> set_some_key cpu 0x8
        | `D      -> set_some_key cpu 0x9
        | `F      -> set_some_key cpu 0xE
        | `Z      -> set_some_key cpu 0xA
        | `X      -> set_some_key cpu 0x0
        | `C      -> set_some_key cpu 0xB
        | `V      -> set_some_key cpu 0xF
        | `Escape -> Caml.exit 0
        | _       -> ()
      end
    | `Key_up -> set_none_key cpu
    | `Quit   -> Caml.exit 0
    | _       -> ()
  end


let () =
  let argv = Sys.get_argv () in
  if Array.length argv < 2 then begin
    prerr_endline "Usage : main <file name>";
    Caml.exit 2
  end;
  let rom = In_channel.read_all argv.(1) |> Bytes.of_string in
  let cpu = Cpu.create ~rom in
  let renderer = init_graphics () in
  while true do
    if Float.((Unix.gettimeofday() -. !last_tick) >= 1./.360.) then begin
      Cpu.tick cpu;
      clear_graphics renderer;
      draw_graphics cpu renderer;
      handle_event cpu;
      last_tick := Unix.gettimeofday();
    end
  done
