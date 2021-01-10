open Netlist_ast
let print_only = ref false
let number_steps = ref (-1)
let int_of_bool_array = Array.fold_left (fun n b -> 2*n+if b then 1 else 0) 0
let rec p2 = function 0->1|n -> 2*p2 (n-1)
let rec read_input typ = 
  let i = read_line () in
  match typ with
  |TBit -> if String.length i = 1 then VBit (i<>"0") else (print_endline "Wrong input."; read_input typ)
  |TBitArray n -> if String.length i = n then VBitArray (Array.init n (fun k->i.[k]<>'0')) else (print_endline "Wrong input."; read_input typ)
let print_bit b = if b then print_char '1' else print_char '0'
let print_output = function
  |VBit b -> print_bit b
  |VBitArray a -> Array.iter print_bit a
let rec loop p vars rom ram ntot = function
  |0 -> ()
  |n -> begin let read_arg = function Avar x->Hashtbl.find vars x|Aconst k -> k in (*renvoie la valeur d'un argument*)
    let as_array c = match read_arg c with VBit b -> [|b|] | VBitArray b -> b in
    print_endline ("Step "^string_of_int (ntot+1-n)); List.iter (fun x-> print_string (x^"? "); Hashtbl.add vars x (read_input (Env.find x p.p_vars))) p.p_inputs;
    let we = ref (Aconst (VBit false)) in let wa = ref (Aconst (VBitArray [||])) in let wd = ref (Aconst (VBit false)) in 
    List.iter (fun (x,e) -> let v = match e with
      | Earg a -> read_arg a
      | Ereg y -> Hashtbl.find vars y
      | Enot a -> let VBit b = read_arg a in VBit (not b)
      | Ebinop (bi,a,b) -> let VBit aa, VBit bb = read_arg a, read_arg b in (match bi with
        |Or -> VBit (aa || bb)
        |Xor -> VBit ((aa || bb) && not(aa && bb))
        |And -> VBit (aa && bb)
        |Nand -> VBit (not(aa && bb))) 
      | Emux (a,b,c) -> let VBit s = read_arg a in if s then read_arg b else read_arg c
      | Erom (n,m,r) -> let a = as_array r in rom.(int_of_bool_array a)
      | Eram (n,m,r,b,c,d) -> we:=b; wa:=c; wd:=d;
        let a = as_array r in ram.(int_of_bool_array a)
      | Econcat (c,d) -> let a,b = as_array c, as_array d in VBitArray (Array.append a b)
      | Eslice (m,n,c) -> let a = as_array c in VBitArray (Array.sub a m (n-m+1))
      | Eselect (n,c) -> let a = as_array c in VBit a.(n)
    in Hashtbl.add vars x v) p.p_eqs;
    print_newline (); List.iter (fun x -> print_string (x^": "); print_output (Hashtbl.find vars x); print_newline ()) p.p_outputs;
    let VBit wee = read_arg !we in let waa = as_array !wa in let wdd = read_arg !wd in
    if wee then ram.(int_of_bool_array waa)<-wdd;
    (* gestion de la ram : il ne peut y avoir qu'une seule instruction ram dans le programme car on ne peut pas écrire à deux endroits à la fois*)
    loop p vars rom ram ntot (n-1) end

let simulator program n = let lrom = ref 0 in let wrom = ref 0 in let lram = ref 0 in let wram = ref 0 in 
  List.iter (fun (x,e) -> match e with Erom (n,m,r)-> lrom:=p2 n;wrom:=m | Eram (n,m,r,b,c,d)-> lram:=p2 n;wram:=m | _->()) program.p_eqs;
  let h = Hashtbl.create 0 in Env.iter (fun x t -> Hashtbl.add h x (match t with TBit -> VBit false | TBitArray m -> VBitArray (Array.make m false))) program.p_vars;
  loop program h (Array.init !lrom (**)(let r = ref false in fun k -> if !r then VBitArray (Array.make !wrom false) else begin
    print_string ("rom.("^(string_of_int k)^")?"); let t = read_input (TBitArray !wrom) in (let VBitArray a = t in if Array.for_all (fun b->not(b)) a then r:=true);t end)(**))
    (Array.make !lram (VBitArray (Array.make !wram false))) 
    n n


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
