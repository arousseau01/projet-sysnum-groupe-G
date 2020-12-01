open Netlist_ast
let print_only = ref false
let number_steps = ref (10)

let cnot v = match v with 
  |VBit b -> VBit (not b)
  |VBitArray t -> let n=Array.length t in 
                  let t'=Array.make n true in 
                    for i=0 to n-1 do
                      t'.(i)<-not(t.(i))
                    done;
                  VBitArray t'

let cbinop bin v1 v2 = match v1,v2 with 
  |VBit b1, VBit b2 ->
    begin match bin with 
      |Or ->  VBit(b1 || b2)
      |Xor -> VBit((b1 && (not b2))||(b2 && (not b1)))
      |And ->  VBit(b1 && b2)
      |Nand ->  VBit(not (b1 && b2))
    end;
  |VBitArray t1,VBitArray t2 when Array.length t1=Array.length t2 -> 
    let n=Array.length t1 in let t = Array.make n true in 
    for i=0 to n-1 do 
      begin match bin with 
        |Or ->  t.(i)<-(t1.(i)||t2.(i))
        |Xor -> t.(i)<-((t1.(i) && (not t2.(i)))||(t2.(i) && (not t1.(i))))
        |And -> t.(i)<-(t1.(i) && t2.(i))
        |Nand ->  t.(i)<-(not (t1.(i) && t2.(i)))
      end;
    done;
    VBitArray t;
  |_,_ -> failwith "pas de même longueur"

let cmux v1 v2 v3 = match v1 with 
  |VBit b -> if b then v2 else v3
  |_ -> failwith "multiplexeur prend un booléen"

let rec cconcat v1 v2 = match v1,v2 with
  |VBitArray t1, VBitArray t2 -> 
    let t = Array.make (Array.length t1 +Array.length t2) true in 
    for i = 0 to Array.length t1 -1 do
      t.(i)<-t1.(i)
    done;
    for i = 0 to Array.length t2 -1 do
      t.(i+Array.length t1)<-t2.(i)
    done; 
    VBitArray t;
  |VBit b1,_ -> cconcat (VBitArray [|b1|]) v2
  |_,VBit b2 -> cconcat v1 (VBitArray [|b2|])

let cslice i1 i2 v = match v with 
  |VBitArray t -> 
    if (0<=i1 && i1<=i2 && i2<Array.length t) 
    then (let t' = Array.make (i2-i1+1) true in 
      for i = 0 to Array.length t'-1 do
        t'.(i)<-t.(i+i1)
      done; 
    VBitArray t';) 
    else failwith "mauvais indices"
  |_ -> failwith "slice prend des tableaux"

let cselect i v = match v with 
  |VBitArray t -> 
    if (0<=i && i<Array.length t) 
    then VBit t.(i) 
    else failwith "indice en dehors"
  |VBit b when i=0 -> VBit b
  |_ -> failwith "select prend des tableaux"

let bin_to_int v = match v with
  |VBitArray t -> let s=ref(0) in 
  for i=0 to Array.length t -1 do 
    s:=2*(!s)+(if t.(i) then 1 else 0); 
  done; 
  !s;
  |VBit b -> if b then 1 else 0

let bin_to_tab v = match v with
|VBitArray t -> let t'=Array.make (Array.length t) 0 in 
  for i=0 to Array.length t -1 do 
    t'.(i)<-(if t.(i) then 1 else 0); 
  done; 
  t';
|_ -> failwith "bin_to_tab"

let tab_to_bin v = match v with
|t -> let t'=Array.make (Array.length t) true in 
  for i=0 to Array.length t -1 do 
    t'.(i)<-(t.(i)=1); 
  done;
  VBitArray t'

let string_to_bin s = 
  if String.length s = 1 
    then (if s="0" then VBit false
      else if s="1" then VBit true 
        else failwith "input doit contenir 0 et 1 seulement")
      else 
  let t=Array.make (String.length s) true in 
  for i=0 to String.length s -1 do
    if s.[i]='0'
    then t.(i)<- false
    else if s.[i]='1'
         then t.(i)<- true
         else failwith "input doit contenir 0 et 1 seulement" 
  done;
  VBitArray t
  
let rec longword_rom l = match l with 
  |[] -> 0
  |t::q -> let (_,exp)=t in 
    begin match exp with 
      |Erom(_,lwd,_)-> lwd; 
      |_-> longword_rom q; 
    end

let initialiser_rom p = 
  let lwd=longword_rom p in
  let source = open_in "rom.bin" in 
  let l = ref([]) in 
if lwd<>0 
then 
  begin
  let remplir () = 
    try
      while true do 
        let l2=ref([]) in
        for i = 1 to lwd do 
          l2:=((let c=input_char source in 
                if c='0'
                then 0 
                else if c='1' 
                     then 1 
                     else failwith "rom.bin incorrect")::(!l2))
        done;
        l2:=List.rev(!l2);
        let t2=Array.of_list (!l2) in
        l:=(tab_to_bin t2)::(!l);
      done;
    with End_of_file -> ();
  in remplir ();
  let t=Array.of_list (!l) in
  t 
  end
else [||]

let rec puissance a n = match n with 
  |0 -> a
  |_ -> a*(puissance a (n-1))

let rec longram l = match l with 
  |[] -> (0,0)
  |t::q -> let (_,exp) = t in
    begin match exp with 
      |Eram(lad,lwd,_,_,_,_)-> (lad,lwd); 
      |_-> longram q
    end

let initialiser_ram p = 
let lad,lwd=longram p in 
Array.make (puissance 2 lad) (VBitArray (Array.make lwd false))

let calculer_arg a r tab = try match a with 
  |Avar x -> Env.find x tab.(r) 
  |Aconst c -> c
with Not_found -> failwith "calculer_arg"

let val_init a = match a with 
  |TBit -> VBit false
  |TBitArray n -> VBitArray (Array.make n false)

let print_value v = match v with 
  |VBit b -> 
    if b then print_int 1 else print_int 0
  |VBitArray t -> 
    for i=0 to Array.length t -1 do
      if t.(i) then print_int 1 else print_int 0 
    done

let rec print_list l = match l with
    |[]->print_int 1
    |t::q-> let (x,e)=t in print_string x; print_list q

let simulator program number_steps = 
  let tab= [|Env.empty ;Env.empty|] in 
  Env.iter (fun k a -> tab.(0) <- Env.add k (val_init a) tab.(0)) program.p_vars; 
  Env.iter (fun k a -> tab.(1) <- Env.add k (val_init a) tab.(1)) program.p_vars; 
  let rom = initialiser_rom program.p_eqs in
  let ram = initialiser_ram program.p_eqs in
  let ajram=ref(Aconst (VBit true),Aconst (VBit true),Aconst (VBit true)) in 

let calculer exp r tab = try match exp with 
  | Earg a -> calculer_arg a r tab
  | Ereg i -> Env.find i tab.((r+1) mod 2)
  | Enot a -> cnot (calculer_arg a r tab)
  | Ebinop (bin , a1 , a2) -> cbinop bin (calculer_arg a1 r tab) (calculer_arg a2 r tab)
  | Emux (a1 , a2 , a3) -> cmux (calculer_arg a1 r tab) (calculer_arg a2 r tab) (calculer_arg a3 r tab)
  | Erom (_ ,_ , adrd) -> rom.(bin_to_int (calculer_arg adrd r tab))
  | Eram (_ ,_ , adrd , wren , wrad , da) -> ajram:=(wren , wrad , da); 
                                             ram.(bin_to_int (calculer_arg adrd r tab))
  | Econcat (a1 , a2) -> cconcat (calculer_arg a1 r tab) (calculer_arg a2 r tab)
  | Eslice (i1 , i2 , a) -> cslice i1 i2 (calculer_arg a r tab)
  | Eselect (i , a) -> cselect i (calculer_arg a r tab)
with Not_found -> failwith "ereg" 
in

  for i=0 to number_steps-1 do
    let r=i mod 2 in 
    Printf.printf "Etape "; print_int i; print_string "\n"; flush stdout;
    List.iter (function k -> print_string k; print_string "=? "; flush stdout; tab.(r) <- Env.add k (string_to_bin (Scanf.bscanf Scanf.Scanning.stdin "%s\n" (function x -> x))) tab.(r)) 
              program.p_inputs;
    List.iter (function eq -> let (x,exp)=eq in let v=calculer exp r tab in tab.(r)<-Env.add x v tab.(r)) 
              program.p_eqs;
    let (wren , wrad , da)=(!ajram) in 
    let wren'=calculer_arg wren r tab in 
    let wrad'=calculer_arg wrad r tab in
    let da'=calculer_arg da r tab in
    begin match wren' with 
      |VBit false -> ()
      |VBit true -> ram.(bin_to_int wrad') <- da
      |_ -> failwith "mauvais test pour la ram"  end;
    List.iter (function x -> 
      try let v = Env.find x tab.(r) in 
      print_string x;
      print_string "=";
      print_value v;
      print_string "\n" with Not_found -> failwith "print" ) 
              program.p_outputs;
      print_string "\n";
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
    "";;

main ()