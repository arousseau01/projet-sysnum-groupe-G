open Instr

let codegen_instr i = 
  let opc = opcode i.iname in 
  let rd = reg_num i.rd in
  let rs1 = reg_num i.rs1 in
  let rs2 = reg_num i.rs2 in
  let f3 = func3 i.iname in
  let f7 = func7 i.iname in 
  let imm = imm_bits i.imm in
  imm @ func7 @ func3 @ rs2 @ rs1 @ rd @ opc

let codegen_prog i_list = 
  List.map codegen_instr i_list

let pp_bin_instr fmt i =
  let segments = [16, 1, 3, 3, 3, 3, 3] in
  let buf = Buffer.create 33 in
  let rec loop = function
    | [], _ -> Buffer.add_string buf "\n"
    | d :: digits, 0 :: segments ->
      Buffer.add_string buf " ";
      Buffer.add_string buf (int_to_string d);
      loop (digits, segments)
    | d :: digits, s :: segments ->
      Buffer.add_string buf (int_to_string d);
      loop (digits, (s-1) :: segments)
  in
  loop (i, segments);
  Format.fprintf fmt (Buffer.contents buf) 

let print_bin_prog i_list =
  let fmt = Format.std_formatter in
  List.iter 
    (fun i -> Format.fprintf fmt "%a\n" pp_bin_instr i)
     i_list