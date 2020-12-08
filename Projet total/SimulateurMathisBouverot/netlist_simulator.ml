open Netlist_ast

let print_only = ref false
let number_steps = ref (-1)
let rom_file = ref ""

(* evaluate an argument *)
let eval ctx = function
  | Avar x -> Context.get_value ctx x
  | Aconst v -> v

(* execute an instruction *)
let instr ctx (x, exp) = 
  let v = match exp with
    | Earg a -> eval ctx a
    | Enot a -> 
      let v = eval ctx a in
      begin match v with
        | VBit b -> VBit (not b)
        | VBitArray ba -> VBitArray (Array.map (not) ba)
      end
    | Ebinop (op, a1, a2) ->
      let v1 = eval ctx a1 in
      let v2 = eval ctx a2 in
      let f = begin match op with
        | Or -> (||)
        | Xor -> (<>)
        | And -> (&&)
        | Nand -> (fun x y -> not (x && y))
      end in
      begin match v1, v2 with
        | VBit b1, VBit b2 -> VBit (f b1 b2)
        | VBitArray ba1, VBitArray ba2 when (Array.length ba1) = (Array.length ba2) ->
          VBitArray (Array.map2 f ba1 ba2)
        | _ -> failwith "unsupported operand types for binop"
      end
    | Ereg y -> Context.get_value ctx y
    | Erom (_, _, read_addr) ->
      Context.read_rom ctx (eval ctx read_addr)
    | Eram (_, _, read_addr, _, _, _) ->
      Context.read_ram ctx (eval ctx read_addr)
    | Econcat (a1, a2) -> 
      let v1 = eval ctx a1 in
      let v2 = eval ctx a2 in
      begin match v1, v2 with
        | VBit b1, VBit b2 -> VBitArray [|b1 ; b2|]
        | VBit b1, VBitArray ba2 -> VBitArray (Array.append [|b1|] ba2)
        | VBitArray ba1, VBit b2 -> VBitArray (Array.append ba1 [|b2|])
        | VBitArray ba1, VBitArray ba2 -> VBitArray (Array.append ba1 ba2)
      end
    | Eselect (i, a) -> 
      let v1 = eval ctx a in
      begin match v1 with
        | VBit b when i = 0 -> VBit b
        | VBitArray ba when i < (Array.length ba) -> VBit ba.(i)
        | _ -> failwith (Printf.sprintf "out of bounds when writing to %s\n" x)
      end
    | Eslice (i1, i2, a) -> 
      let v1 = eval ctx a in
      begin match v1 with
        | VBit b when i1 = 0 && i2 = 0 -> VBit b
        | VBitArray ba when i1 < (Array.length ba) && i2 < (Array.length ba) ->
          VBitArray (Array.sub ba i1 (i2 - i1 + 1))
        | _ -> failwith (Printf.sprintf "out of bounds when writing to %s\n" x)
      end
    | Emux(cond, a1, a0) ->
      let v = eval ctx cond in
      begin match v with
        | VBit true | VBitArray [|true|] -> eval ctx a1
        | VBit false | VBitArray [|false|] -> eval ctx a0
        | _ -> failwith ("multiplexer condition should be one bit long")
      end 
    | _ -> assert false
  in Context.set_value ctx x v

let write ctx (_, exp) = match exp with
  | Eram (_, _, _, en, addr, data) ->
    let write = match eval ctx en with
      | VBit true -> true
      | VBitArray ba when (Array.length ba = 1) && ba.(0) -> true
      | _ -> false
    in
    if write then Context.write_ram ctx (eval ctx addr) (eval ctx data)
  | _ -> ()

let simulator program number_steps =
  let ctx = Context.create program in
  Io.print_greetings ctx;
  let step i =
    Io.read_inputs ctx i program.p_inputs;
    List.iter (instr ctx) program.p_eqs;
    (* perform the writing after everyting else *)
    List.iter (write ctx) program.p_eqs;
    Io.print_outputs ctx i program.p_outputs
  in
  for i = 1 to number_steps do
	  step i;
  done    

let compile filename =
  try
    let p = Netlist.read_file filename in
    begin try
        let p = Scheduler.schedule p in
        simulator p !number_steps
      with
        | Scheduler.Combinational_cycle ->
            Format.eprintf "The netlist has a combinatory cycle.@.";
    end;
  with
    | Netlist.Parse_error s -> Format.eprintf "An error accurred: %s@." s; exit 2

let main () =
  Arg.parse
    ["-n", Arg.Set_int number_steps, "Number of steps to simulate"]
    compile
    ""
;;

main ()
