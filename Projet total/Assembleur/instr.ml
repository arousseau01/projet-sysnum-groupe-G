open Format

type instr_name =
  | ADD | SUB | OR | NAND | XOR | NXOR | AND 
  | NOR | SLL | SRL | SRA | SEQ | SLT
  | ADDI | SUBI | ORI | NANDI | XORI | NXORI | ANDI 
  | NORI | SLLI | SRLI | SRAI | SEQI | SLTI
  | LW | SW | MOV
  | BEQ | BNE | BLE | BLT | BGE | BGT
  | JAL | JALR | JR | JMP 

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
  | ADD  -> "add"
  | SUB  -> "sub"
  | OR   -> "or"
  | NAND -> "nand"
  | XOR  -> "xor"
  | NXOR -> "nxor"
  | AND  -> "and"
  | NOR  -> "nor"
  | SLL  -> "sll"
  | SRL  -> "srl"
  | SRA  -> "sra"
  | SEQ  -> "seq"
  | SLT  -> "slt"

  | ADDI  -> "addi"
  | SUBI  -> "subi"
  | ORI   -> "ori"
  | NANDI -> "nandi"
  | XORI  -> "xori"
  | NXORI -> "nxori"
  | ANDI  -> "andi"
  | NORI  -> "nori"
  | SLLI  -> "slli"
  | SRLI  -> "srli"
  | SRAI  -> "srai"
  | SEQI  -> "seqi"
  | SLTI  -> "slti" 
  
  | LW -> "lw"
  | SW -> "sw"
  | MOV -> "mov"

  | BEQ -> "beq"
  | BNE -> "bne"
  | BLE -> "ble"
  | BLT -> "blt"
  | BGE -> "bge"
  | BGT -> "bgt"

  | JAL  -> "jal"
  | JALR -> "jalr"
  | JR   -> "jr"
  | JMP  -> "jmp"

let reg_to_string = function
  | RZ  -> "%rz"
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

let is_jump = function 
  | JAL | JALR | JR | JMP -> true
  | _ -> false

let imm_version = function
  | ADD  -> ADDI
  | SUB  -> SUBI
  | OR   -> ORI
  | NAND -> NANDI
  | XOR  -> XORI
  | NXOR -> NXORI
  | AND  -> ANDI
  | NOR  -> NORI
  | SLL  -> SLLI
  | SRL  -> SRLI
  | SRA  -> SRAI
  | SEQ  -> SEQI
  | SLT  -> SLTI
  | _    -> assert false

let reg_num = function
  | RZ  -> 0b000
  | RAX -> 0b001
  | RBX -> 0b010
  | RCX -> 0b011
  | RDX -> 0b100
  | REX -> 0b101
  | RFX -> 0b110
  | RGX -> 0b111
 
let func3 = function
  | ADD  -> 0b000
  | SUB  -> 0b000
  | OR   -> 0b100
  | NAND -> 0b100
  | XOR  -> 0b001
  | NXOR -> 0b001
  | AND  -> 0b101
  | NOR  -> 0b101
  | SLL  -> 0b011
  | SRL  -> 0b010
  | SRA  -> 0b010
  | SEQ  -> 0b111
  | SLT  -> 0b110

  | ADDI  -> 0b000
  | SUBI  -> 0b000
  | ORI   -> 0b100
  | NANDI -> 0b100
  | XORI  -> 0b001
  | NXORI -> 0b001
  | ANDI  -> 0b101
  | NORI  -> 0b101
  | SLLI  -> 0b011
  | SRLI  -> 0b010
  | SRAI  -> 0b010
  | SEQI  -> 0b111
  | SLTI  -> 0b110
  
  | LW  -> 0b000
  | SW  -> 0b000

  | BEQ -> 0b100
  | BNE -> 0b101
  | BLE -> 0b110
  | BLT -> 0b010
  | BGE -> 0b011
  | BGT -> 0b111

  | JAL  -> 0b000
  | JALR -> 0b000
  | JR   -> 0b000
  | JMP  -> 0b001

  | _ -> assert false (* Cas du MOV *)

let func7 = function
  | ADD  -> 0b0
  | SUB  -> 0b1
  | OR   -> 0b0
  | NAND -> 0b1
  | XOR  -> 0b0
  | NXOR -> 0b1
  | AND  -> 0b0
  | NOR  -> 0b1
  | SLL  -> 0b0
  | SRL  -> 0b0
  | SRA  -> 0b1
  | SEQ  -> 0b1
  | SLT  -> 0b1

  | ADDI  -> 0b0
  | SUBI  -> 0b1
  | ORI   -> 0b0
  | NANDI -> 0b1
  | XORI  -> 0b0
  | NXORI -> 0b1
  | ANDI  -> 0b0
  | NORI  -> 0b1
  | SLLI  -> 0b0
  | SRLI  -> 0b0
  | SRAI  -> 0b1
  | SEQI  -> 0b1
  | SLTI  -> 0b1
  
  | LW  -> 0b000
  | SW  -> 0b000

  | BEQ -> 0b1
  | BNE -> 0b1
  | BLE -> 0b1
  | BLT -> 0b1
  | BGE -> 0b1
  | BGT -> 0b1

  | JAL  -> 0b0
  | JALR -> 0b0
  | JR   -> 0b0
  | JMP  -> 0b1

  | _ -> assert false (* Cas du MOV *)

let opcode = function
  | ADD  -> 0b011
  | SUB  -> 0b011
  | OR   -> 0b011
  | NAND -> 0b011
  | XOR  -> 0b011
  | NXOR -> 0b011
  | AND  -> 0b011
  | NOR  -> 0b011
  | SLL  -> 0b011
  | SRL  -> 0b011
  | SRA  -> 0b011
  | SEQ  -> 0b011
  | SLT  -> 0b011

  | ADDI  -> 0b001
  | SUBI  -> 0b001
  | ORI   -> 0b001
  | NANDI -> 0b001
  | XORI  -> 0b001
  | NXORI -> 0b001
  | ANDI  -> 0b001
  | NORI  -> 0b001
  | SLLI  -> 0b001
  | SRLI  -> 0b001
  | SRAI  -> 0b001
  | SEQI  -> 0b001
  | SLTI  -> 0b001
  
  | LW  -> 0b000
  | SW  -> 0b010

  | BEQ -> 0b110
  | BNE -> 0b110
  | BLE -> 0b110
  | BLT -> 0b110
  | BGE -> 0b110
  | BGT -> 0b110

  | JAL  -> 0b111
  | JALR -> 0b101
  | JR   -> 0b100
  | JMP  -> 0b110

  | _ -> assert false (* Cas du MOV *)

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
      fprintf fmt "rd=%s rs1=%s\n"
        (reg_to_string instr.rd)
        (reg_to_string instr.rs1)
    | SW -> 
      fprintf fmt "rs1=%s rs2=%s\n"
        (reg_to_string instr.rs1)
        (reg_to_string instr.rs2)
    | n when is_branch n ->
      fprintf fmt "rs1=%s rs2=%s label=%s imm=%d\n"
        (reg_to_string instr.rs1)
        (reg_to_string instr.rs2)
        instr.label
        instr.imm
    | JAL ->
      fprintf fmt "rd=%s label=%s imm=%d\n"
        (reg_to_string instr.rd)
        instr.label
        instr.imm
    | JALR -> 
      fprintf fmt "rd=%s rs1=%s\n"
        (reg_to_string instr.rd)
        (reg_to_string instr.rs1)
    | JR -> 
      fprintf fmt "rs1=%s\n"
        (reg_to_string instr.rs1)
    | JMP -> 
      fprintf fmt "label=%s imm=%d\n"
        instr.label
        instr.imm
    | _ -> failwith "mov"

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