exception InvalidInput

(* i is a non negative integer *)
let digits i =
  if i = 0 then [0] else
  let rec aux acc = function
	| 0 -> acc
	| i -> aux ((i mod 10) :: acc) (i / 10)
  in
	aux [] i 

let read_bit_array n =
  let s = read_line () in
  if String.length s <> n then raise InvalidInput else 
  let ba = Array.make n false in
  for i = 0 to n - 1 do
    match s.[i] with
      | '0' -> ba.(i) <- false
      | '1' -> ba.(i) <- true
      | _ -> raise InvalidInput
  done;
  ba


let print_greetings ctx =
  let rom_addr, rom_word = Context.rom_size ctx in
  let ram_addr, ram_word = Context.ram_size ctx in
  Printf.printf "*****\nNETLIST SIMULATOR\n";
  Printf.printf "ROM: addr_size=%d word_size=%d\n" rom_addr rom_word;
  Printf.printf "RAM: addr_size=%d word_size=%d\n" ram_addr ram_word;
  Printf.printf "*****\n"

let rec read_inputs ctx step inputs =
  Printf.printf "Inputs for step %d (binary)\n" step;
  let read_single x =
	  match Context.get_type ctx x with
      | TBit ->
        Printf.printf "=> %s(1 digit):" x;
        let ba = read_bit_array 1 in
        Context.set_value ctx x (VBit ba.(0))
      | TBitArray n -> 	
        Printf.printf "=> %s(%d digits):" x n;
        let ba = read_bit_array n in
        Context.set_value ctx x (VBitArray ba)
  in
  try 
    List.iter read_single inputs
  with InvalidInput ->
    Printf.printf "[INVALID INPUT]\n";
	  read_inputs ctx step inputs

let bool_to_char = function
  | true -> '1'
  | false -> '0'

let print_outputs ctx step outputs  =
  Printf.printf "Outputs of step %d\n" step;
  let print_single x =
    let str = match Context.get_value ctx x with
	  | VBit b -> String.make 1 (bool_to_char b)
	  | VBitArray ba -> 
		  let buf = Buffer.create (Array.length ba) in 
      Array.iter (fun b -> Buffer.add_char buf (bool_to_char b)) ba;
      Buffer.contents buf
	in
	  Printf.printf "=> %s: %s\n" x str 
  in 
    List.iter print_single outputs


