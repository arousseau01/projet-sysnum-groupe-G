open Instr

let codegen_instr i = 
  let opc = opcode i.iname in 
  let rd = reg_num i.rd in
  let rs1 = reg_num i.rs1 in
  let rs2 = reg_num i.rs2 in
  let f3 = func3 i.iname in
  let f7 = func7 i.iname in 
  let imm = imm_to_bin i.imm in
  imm @ f7 @ f3 @ rs2 @ rs1 @ rd @ opc

let codegen_prog i_list = 
  List.map codegen_instr i_list

let write_to_file fname i_list =
  let f = open_out fname in
  let write_instr i =
    let rec loop = function
      | [] -> Printf.fprintf f "\n"
      | d :: digits ->
        let str = match d with
          | 0 -> "0"
          | 1 -> "1"
          | _ -> assert false
        in
        Printf.fprintf f "%s" str;
        loop digits
    in
    loop i
  in
  List.iter write_instr i_list;
  close_out f

(***** PRINTING *****)

let spaces = 3
let pp_bin_instr fmt i =
  let segments = [16; 1; 3; 3; 3; 3; 3] in
  let buf = Buffer.create 33 in
  let rec loop = function
    | [], _ -> ()
    | d :: digits, 1 :: segments ->
      Buffer.add_string buf (string_of_int d);
      for _ = 1 to spaces do
        Buffer.add_string buf " ";
      done;
      loop (digits, segments)
    | d :: digits, s :: segments ->
      Buffer.add_string buf (string_of_int d);
      loop (digits, (s-1) :: segments)
    | _ -> assert false (* the sum of the segments should be longer than the digits *)
  in
  loop (i, segments);
  Format.fprintf fmt "%s" (Buffer.contents buf) 


(* pad with spaces *)
let pad_string size str =
  let buf = Buffer.create 8 in
  Buffer.add_string buf str;
  for _ = 1 to size - (String.length str) do
    Buffer.add_string buf " "
  done;
  Buffer.contents buf

let print_bin_prog i_list =
  let fmt = Format.std_formatter in
  Format.fprintf fmt "INSTRUCTIONS (bin):\n";
  Format.fprintf fmt "%s%s%s%s%s%s%s\n"
    (pad_string (16 + spaces) "imm")
    (pad_string (1 + spaces) "f7")
    (pad_string (3 + spaces) "f3")
    (pad_string (3 + spaces) "rs2")
    (pad_string (3 + spaces) "rs1")
    (pad_string (3 + spaces) "rd")
    (pad_string (3 + spaces) "opc");
  List.iter 
    (fun i -> Format.fprintf fmt "%a\n" pp_bin_instr i)
     i_list