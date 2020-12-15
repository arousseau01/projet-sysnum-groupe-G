open Format

type instr_name =
  | ADD | SUB | OR | NAND | XOR | NXOR | AND 
  | NOR | SLL | SRL | SRA | SEQ | SLT
  | ADDI | SUBI | ORI | NANDI | XORI | NXORI | ANDI 
  | NORI | SLLI | SRLI | SRAI | SEQI | SLTI
  | LW | SW | MOV
  | BEQ | BNE | BLE | BLT | BGE | BGT 

type reg =
  | RZ
  | RAX
  | RBX
  | RCX
  | RDX
  | REX
  | RFX
  | RGX

type instr =
{
  mutable ipos : int ; (* offset in the source file *)
  mutable iname : instr_name ;
  mutable rd : reg ; 
  mutable rs1 : reg ; 
  mutable rs2 : reg ; 
  mutable imm : int ; (* 16 bits *)
  mutable label : string (* utilisé dans les instructions branch et jump *)
}

type section = S_DATA | S_TEXT

type label = {
  mutable lpos : int ; (* offset in the source file *)
  mutable lname : string ;
  mutable section : section
}
type prog = instr list * label list


let create_instr name = {
  ipos = -1 ;
  iname = name ;
  rd = RZ ;
  rs1 = RZ ;
  rs2 = RZ ;
  imm = 0 ;
  label = ""
}

let instr_to_string = function
  | ADD -> "add"
  | SUB -> "sub"
  | OR -> "or"
  | NAND -> "nand"
  | XOR -> "xor"
  | NXOR -> "nxor"
  | AND -> "and"
  | NOR -> "nor"
  | SLL -> "sll"
  | SRL -> "srl"
  | SRA -> "sra"
  | SEQ -> "seq"
  | SLT -> "slt"

  | ADDI -> "addi"
  | SUBI -> "subi"
  | ORI -> "ori"
  | NANDI -> "nandi"
  | XORI -> "xori"
  | NXORI -> "nxori"
  | ANDI -> "andi"
  | NORI -> "nori"
  | SLLI -> "slli"
  | SRLI -> "srli"
  | SRAI -> "srai"
  | SEQI -> "seqi"
  | SLTI -> "slti" 
  
  | LW -> "lw"
  | SW -> "sw"
  | MOV -> "mov"

  | BEQ -> "beq"
  | BNE -> "bne"
  | BLE -> "ble"
  | BLT -> "blt"
  | BGE -> "bge"
  | BGT -> "bgt"

let reg_to_string = function
  | RZ -> "%rz"
  | RAX -> "%rax"
  | RBX -> "%rbx"
  | RCX -> "%rcx"
  | RDX -> "%rdx"
  | REX -> "%rex"
  | RFX -> "%rfx"
  | RGX -> "%rgx"

let section_to_string = function
  | S_DATA -> "data"
  | S_TEXT -> "text"

let is_reg_arith = function
  | ADD | SUB | OR | NAND | XOR | NXOR | AND 
  | NOR | SLL | SRL | SRA | SEQ | SLT -> true
  | _ -> false

let is_imm_arith = function
  | ADDI | SUBI | ORI | NANDI | XORI | NXORI | ANDI 
  | NORI | SLLI | SRLI | SRAI | SEQI | SLTI -> true
  | _ -> false

let is_arith name = 
  is_reg_arith name || is_imm_arith name

let is_branch = function
  | BEQ | BNE | BLE | BLT | BGE | BGT -> true
  | _ -> false

let imm_version = function
  | ADD -> ADDI
  | SUB -> SUBI
  | OR -> ORI
  | NAND -> NANDI
  | XOR -> XORI
  | NXOR -> NXORI
  | AND -> ANDI
  | NOR -> NORI
  | SLL -> SLLI
  | SRL -> SRLI
  | SRA -> SRAI
  | SEQ -> SEQI
  | SLT -> SLTI
  | _ -> assert false

let reg_num = function
  | RZ -> 0b000
  | _ -> failwith "not implemented"
 
let func3 = function
  | ADD -> failwith "not implemented"
  | _ -> failwith "not implemented"

(* pareil pour func7 et opcode *)

(***** PRINTING *****)

let pp_pos fmt pos =
  fprintf fmt "[%d]  " pos

let pp_instr fmt instr =
  fprintf fmt "%a%s " 
    pp_pos instr.ipos 
    (instr_to_string instr.iname);
  match instr.iname with
    | n when is_reg_arith n ->
      fprintf fmt "rd=%s rs1=%s rs2=%s\n" 
        (reg_to_string instr.rd)
        (reg_to_string instr.rs1)
        (reg_to_string instr.rs2)
    | n when is_imm_arith n ->
      fprintf fmt "rd=%s rs1=%s imm=%s\n"
        (reg_to_string instr.rd)
        (reg_to_string instr.rs1)
        (string_of_int instr.imm)
    | LW -> 
      fprintf fmt "rd=%s rs1=%s imm=%d\n"
        (reg_to_string instr.rd)
        (reg_to_string instr.rs1)
        instr.imm
    | SW -> 
      fprintf fmt "rs1=%s rs2=%s imm=%d\n"
        (reg_to_string instr.rs1)
        (reg_to_string instr.rs2)
        instr.imm
    | n when is_branch n ->
      fprintf fmt "rs1=%s rs2=%s label=%s imm=%d\n"
        (reg_to_string instr.rs1)
        (reg_to_string instr.rs2)
        instr.label
        instr.imm
    | _ -> failwith "not implemented"

let pp_label fmt lab =
  fprintf fmt "label %s pos=%d section=%s\n"
    lab.lname
    lab.lpos
    (section_to_string lab.section)

let print_prog (i_list, lab_list) =
  let fmt = Format.std_formatter in
  fprintf fmt "Instructions:\n";
  List.iter (pp_instr fmt) i_list;
  fprintf fmt "Labels:\n";
  List.iter (pp_label fmt) lab_list