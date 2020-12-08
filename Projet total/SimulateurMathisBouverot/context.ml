open Netlist_ast

module Memory = struct
  (* can be a ram or a rom *)
  type t =
  {
    addr_size : int ;
    word_size : int ;
    data : bool array
  }

  let create addr_size word_size =
    { 
      addr_size = addr_size ; 
      word_size = word_size ;
      data = Array.make ((1 lsl addr_size) * word_size) false
    }
  
  let bit_array_to_int ba =
    let n = ref 0 in
    for i = 0 to (Array.length ba) - 1 do
      if ba.(i) then n := 1 + !n * 2
      else n := !n * 2
    done;
    !n

  let decode r addr = 
    match addr with
      | VBit b when r.addr_size = 1 -> 
        if b then r.word_size else 0
      | VBitArray ba when (Array.length ba) = r.addr_size ->
        r.word_size * (bit_array_to_int ba)
      | _ -> failwith "incompatible address size in memory"

  let read r addr =
    let pos = decode r addr in
    if pos + r.word_size > Array.length r.data
    then failwith "Read memory: invalid address"
    else
      VBitArray (Array.sub r.data pos r.word_size)

  let write r addr word =
    match word with
      | VBit b when r.word_size = 1 ->
        let pos = decode r addr in
        r.data.(pos) <- b;
      | VBitArray ba when (Array.length ba) = r.word_size ->
        let start = decode r addr in
        for i = 0 to r.word_size - 1 do
          r.data.(start + i) <- ba.(i);
        done
      | _ -> failwith "incompatible word size"
end

type t =
{
  types : ty Env.t ;
  mutable values : value Env.t ;
  rom : Memory.t ;
  ram : Memory.t
}
(*
(* initialize the rom from what is read in file
 * the file should contain 2**rom.addr_size blocks 
 * of rom.word_size 0s and 1s, each on a different line *)
let flash_rom ctx file =
  let c = open_in file in
  let read_block start =
    let s = input_line c in
    if String.length s <> ctx.rom.word_size
    then failwith "Error reading rom : block size doesn't match word size"
    else
    for i = 0 to ctx.rom.word_size - 1 do
      let b = match s.[i] with
        | '0' -> 0
        | '1' -> 1
        | c -> failwith (Printf.sprintf "Error reading rom : invalid character '%c'" c)
      in
      ctx.rom.data.(!start + i) <- b
    done
  in
  for i = 0 to 2^ctx.rom.addr_size - 1 do
    try read_input (i * ctx.rom.word_size)
    with End_of_file -> raise "Error reading rom : not enough blocks in file"
  done
*)

let create_rom prg =
  let rec search = function
	  | (_, (Erom (addr_size, word_size, _))) :: _ ->
	    Memory.create addr_size word_size 
	  | _ :: eqs -> search eqs
  	| [] -> Memory.create 0 0 (* dummy memory *)
  in
	  search prg.p_eqs

let create_ram prg =
  let rec search = function
	  | (_, (Eram (addr_size, word_size, _, _, _, _))) :: _ ->
	    Memory.create addr_size word_size
	  | _ :: eqs -> search eqs
	  | [] -> Memory.create 0 0 (* dummy memory *)
  in
	  search prg.p_eqs

let create prg =
  let ctx = {
    types = prg.p_vars ;
    values = Env.empty ;
    rom = create_rom prg ;
    ram = create_ram prg
  }
  in
  let init_var (x, ty) = 
    let new_values =
      match ty with
        | TBit -> Env.add x (VBit false) ctx.values
        | TBitArray n -> Env.add x (VBitArray (Array.make n false)) ctx.values
    in
    ctx.values <- new_values
  in
  Seq.iter init_var (Env.to_seq ctx.types);
  ctx
 
let set_value ctx x v =
  if not (Env.mem x ctx.types) 
  then failwith (Printf.sprintf "Unknown variable %s" x)
  else if not (Env.mem x ctx.values) 
  then ctx.values <- Env.add x v ctx.values
  else 
  let t = Env.find x ctx.types in
  begin match t, v with
    | TBit, VBit _ -> ()
    | TBitArray 1, VBit _ -> ()
    | TBitArray n, VBitArray ba when (Array.length ba) = n -> ()
    | _ -> failwith (Printf.sprintf "error writing to %s" x)
  end;
  ctx.values <- Env.add x v ctx.values

let get_value ctx x =
  Env.find x ctx.values

let get_type ctx x =
  Env.find x ctx.types

let rom_size ctx =
  ctx.rom.addr_size, ctx.rom.word_size

let ram_size ctx =
  ctx.ram.addr_size, ctx.ram.word_size



let read_rom ctx addr = 
  Memory.read ctx.rom addr

let read_ram ctx addr =
  Memory.read ctx.ram addr

let write_ram ctx addr word =
  Memory.write ctx.ram addr word

