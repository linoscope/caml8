open Base
open Stdint

type t = {
  memory : Memory.t;
  registers : Registers.t;
  mutable i : uint16;
  mutable sp : uint16;
  mutable pc : uint16;
  mutable dt : uint8;
  mutable st : uint8;
  mutable key_state : uint8 option;
  gfx : bool array;
}

let create ~rom =
  let rom_base = Uint16.of_int 0x200 in
  let memory = Memory.create () in
  Memory.load memory ~src:Font_sprites.bytes ~dst_pos:Uint16.zero;
  Memory.load memory ~src:rom ~dst_pos:rom_base;
  {
    memory;
    registers = Registers.create ();
    i = Uint16.zero;
    sp = Uint16.of_int 0xfa0;
    pc = rom_base;
    dt = Uint8.zero;
    st = Uint8.zero;
    key_state = None;
    gfx = Array.create ~len:(64 * 32) false;
  }

type next_pc = Next | Skip | Jump of uint16

let tick t =
  let read_register t vx = Registers.(value t.registers vx) in
  let write_register t vx x = Registers.(set t.registers vx x) in
  let instruction = Instruction.read ~memory:t.memory ~pc:t.pc in
  let next_pc = match instruction with
    | Cls ->
      Array.fill t.gfx ~pos:0 ~len:(Array.length t.gfx) false;
      Next
    | Ret ->
      let ret_addr = Memory.read_uint16 t.memory ~pos:t.sp in
      t.sp <- Uint16.(t.sp - of_int 2);
      Jump ret_addr
    | Jp nnn ->
      Jump nnn
    | Jp_v0 nnn ->
      let v0 = read_register t Registers.v0 in
      Jump Uint16.(of_uint8 v0 + nnn)
    | Call nnn ->
      t.sp <- Uint16.(t.sp + of_int 2);
      Memory.write_uint16 t.memory ~pos:t.sp Uint16.(t.pc + of_int 2);
      Jump nnn
    | Se_vx_nn (vx, nn) ->
      let vx = read_register t vx in
      if Uint8.compare vx nn = 0 then Skip else Next
    | Se_vx_vy (vx, vy) ->
      let vx = read_register t vx in
      let vy = read_register t vy in
      if Uint8.compare vx vy = 0 then Skip else Next
    | Sne_vx_nn (vx, nn) ->
      let vx = read_register t vx in
      if Uint8.compare vx nn <> 0 then Skip else Next
    | Sne_vx_vy (vx, vy) ->
      let vx = read_register t vx in
      let vy = read_register t vy in
      if Uint8.compare vx vy <> 0 then Skip else Next
    | Skp vx ->
      let vx = read_register t vx in
      begin match t.key_state with
        | Some k when Uint8.compare vx k = 0 -> Skip
        | None
        | Some _ -> Next
      end
    | Sknp vx ->
      let vx = read_register t vx in
      begin match t.key_state with
        | Some k when Uint8.compare vx k <> 0 -> Skip
        | None
        | Some _ -> Next
      end
    | Waitkey vx ->
      begin match t.key_state with
        | None -> Jump t.pc
        | Some k -> write_register t vx k; Next
      end
    | Ld_vx_nn (vx, nn) ->
      write_register t vx nn;
      Next
    | Ld_vx_vy (vx, vy) ->
      let vy = read_register t vy in
      write_register t vx vy;
      Next
    | Ld_vx_dt vx ->
      write_register t vx t.dt;
      Next
    | Ld_dt_vx vx ->
      let vx = read_register t vx in
      t.dt <- vx;
      Next
    | Ld_st_vx vx ->
      let vx = read_register t vx in
      t.st <- vx;
      Next
    | Ld_i_nnn nnn ->
      t.i <- nnn;
      Next
    | Spritechar vx ->
      let vx = read_register t vx |> Uint16.of_uint8 in
      t.i <- Uint16.(Uint16.zero + vx * of_int 5);
      Next
    | Regdump vx ->
      let n = Registers.register_to_int vx in
      for i = 0 to n do
        let vi = Registers.register_of_int i |> Registers.value t.registers in
        Memory.write_uint8 t.memory ~pos:Uint16.(t.i + of_int i) vi
      done;
      Next
    | Regload vx ->
      let vx = Registers.register_to_int vx in
      for i = 0 to vx do
        let vi = Registers.register_of_int i in
        let x = Memory.read_uint8 t.memory ~pos:Uint16.(t.i + of_int i) in
        write_register t vi x;
      done;
      Next
    | Add_i_vx vx ->
      let vx = read_register t vx in
      t.i <- Uint16.(t.i + of_uint8 vx);
      Next
    | Add_vx_nn (vx, nn) ->
      let vx' = read_register t vx in
      write_register t vx Uint8.(nn + vx');
      Next
    | Add_vx_vy (vx, vy) ->
      let vx' = read_register t vx in
      let vy = read_register t vy in
      let sum = (Uint8.to_int vx') + (Uint8.to_int vy) in
      let carry_flag = if sum < 0xFFF then Uint8.zero else Uint8.one in
      write_register t Registers.vf carry_flag;
      write_register t vx (Uint8.of_int sum);
      Next
    | Sub (vx, vy) ->
      let vx' = read_register t vx in
      let vy' = read_register t vy in
      let borrow_flag = if Uint8.compare vx' vy' <> -1 then Uint8.zero else Uint8.one in
      write_register t Registers.vf borrow_flag;
      write_register t vx Uint8.(vx' - vy');
      Next
    | Subn (vx, vy) ->
      let vx' = read_register t vx in
      let vy' = read_register t vy in
      let borrow_flag = if Uint8.compare vy' vx' <> -1 then Uint8.zero else Uint8.one in
      write_register t Registers.vf borrow_flag;
      write_register t vx Uint8.(vy' - vx');
      Next
    | Or (vx, vy) ->
      let vx' = read_register t vx in
      let vy = read_register t vy in
      write_register t vx Uint8.(logor vx' vy);
      Next
    | And (vx, vy) ->
      let vx' = read_register t vx in
      let vy = read_register t vy in
      write_register t vx Uint8.(logand vx' vy);
      Next
    | Xor (vx, vy) ->
      let vx' = read_register t vx in
      let vy = read_register t vy in
      write_register t vx Uint8.(logxor vx' vy);
      Next
    | Shr vx ->
      let vx' = read_register t vx in
      write_register t vx (Uint8.shift_right vx' 1);
      Next
    | Shl vx ->
      let vx' = read_register t vx in
      write_register t vx (Uint8.shift_left vx' 1);
      Next
    | Bcd vx ->
      let vx = read_register t vx |> Uint8.to_int in
      let ones, tens, hundreds = vx % 10, (vx / 10) % 10, (vx / 100) % 10 in
      Memory.write_uint8 t.memory ~pos:t.i (Uint8.of_int hundreds);
      Memory.write_uint8 t.memory ~pos:Uint16.(t.i + of_int 1) (Uint8.of_int tens);
      Memory.write_uint8 t.memory ~pos:Uint16.(t.i + of_int 2) (Uint8.of_int ones);
      Next
    | Rnd (vx, nn) ->
      let random_u8 = Random.int 255 |> Uint8.of_int in
      write_register t vx Uint8.(logand random_u8 nn);
      Next
    | Drw (vx, vy, n) ->
      let vx = read_register t vx |> Uint8.to_int in
      let vy = read_register t vy |> Uint8.to_int in
      for y = 0 to Uint8.to_int n - 1 do
        let sprite_line = Memory.read_uint8 t.memory ~pos:Uint16.(t.i + of_int y) |> Uint8.to_int in
        for x = 0 to 7 do
          let sprite_bit = (sprite_line lsr (7 - x)) land (0b00000001) = 1 in
          let gfx_i = ((y + vy) % 32) * 64 + (x + vx % 64) in
          if sprite_bit && t.gfx.(gfx_i) then begin
            t.gfx.(gfx_i) <- false;
          end else if sprite_bit then
            t.gfx.(gfx_i) <- true
        done
      done;
      Next
  in
  (* Stdio.printf "x_[I]=%x\n" @@ (Memory.read_uint16 t.memory ~pos:t.i |> Uint16.to_int);
   * Stdio.printf "%s\n" (Instruction.to_string instruction);
   * Stdio.printf "I=%x, pc=%x, sp=%x, " (t.i |> Uint16.to_int) (t.pc |> Uint16.to_int) (t.sp |> Uint16.to_int);
   * Stdio.printf "%s\n" (Registers.dump t.registers);
   * Stdio.printf "key_state=%x\n" (t.key_state |> Option.value ~default:(Uint8.of_int (-1)) |> Uint8.to_int);
   * if Option.is_some t.key_state then
   *   Stdio.printf "Key pressed! %x\n" (t.key_state |> Option.value_exn |> Uint8.to_int);
   * Stdio.printf "[I]=%x\n" @@ (Memory.read_uint16 t.memory ~pos:t.i |> Uint16.to_int); *)

  t.dt <- Uint8.(if (compare t.dt zero) = 0 then zero else t.dt - one);
  match next_pc with
  | Next -> t.pc <- Uint16.(t.pc + of_int 2)
  | Skip -> t.pc <- Uint16.(t.pc + of_int 4)
  | Jump addr -> t.pc <- addr

let get_gfx t = t.gfx

let set_key t k = t.key_state <- k
