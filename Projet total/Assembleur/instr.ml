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
  | RZ  -> [0;0;0]
  | RAX -> [0;0;1]
  | RBX -> [0;1;0]
  | RCX -> [0;1;1]
  | RDX -> [1;0;0]
  | REX -> [1;0;1]
  | RFX -> [1;1;0]
  | RGX -> [1;1;1]
 
let func3 = function
  | ADD  -> [0;0;0]
  | SUB  -> [0;0;0]
  | OR   -> [1;0;0]
  | NAND -> [1;0;0]
  | XOR  -> [0;0;1]
  | NXOR -> [0;0;1]
  | AND  -> [1;0;1]
  | NOR  -> [1;0;1]
  | SLL  -> [0;1;1]
  | SRL  -> [0;1;0]
  | SRA  -> [0;1;0]
  | SEQ  -> [1;1;1]
  | SLT  -> [1;1;0]

  | ADDI  -> [0;0;0]
  | SUBI  -> [0;0;0]
  | ORI   -> [1;0;0]
  | NANDI -> [1;0;0]
  | XORI  -> [0;0;1]
  | NXORI -> [0;0;1]
  | ANDI  -> [1;0;1]
  | NORI  -> [1;0;1]
  | SLLI  -> [0;1;1]
  | SRLI  -> [0;1;0]
  | SRAI  -> [0;1;0]
  | SEQI  -> [1;1;1]
  | SLTI  -> [1;1;0]
  
  | LW  -> [0;0;0]
  | SW  -> [0;0;0]

  | BEQ -> [1;0;0]
  | BNE -> [1;0;1]
  | BLE -> [1;1;0]
  | BLT -> [0;1;0]
  | BGE -> [0;1;1]
  | BGT -> [1;1;1]

  | JAL  -> [0;0;0]
  | JALR -> [0;0;0]
  | JR   -> [0;0;0]
  | JMP  -> [0;0;1]

  | _ -> assert false (* Cas du MOV *)

let func7 = function
  | ADD  -> [0]
  | SUB  -> [1]
  | OR   -> [0]
  | NAND -> [1]
  | XOR  -> [0]
  | NXOR -> [1]
  | AND  -> [0]
  | NOR  -> [1]
  | SLL  -> [0]
  | SRL  -> [0]
  | SRA  -> [1]
  | SEQ  -> [1]
  | SLT  -> [1]

  | ADDI  -> [0]
  | SUBI  -> [1]
  | ORI   -> [0]
  | NANDI -> [1]
  | XORI  -> [0]
  | NXORI -> [1]
  | ANDI  -> [0]
  | NORI  -> [1]
  | SLLI  -> [0]
  | SRLI  -> [0]
  | SRAI  -> [1]
  | SEQI  -> [1]
  | SLTI  -> [1]
  
  | LW  -> [0]
  | SW  -> [0]

  | BEQ -> [1]
  | BNE -> [1]
  | BLE -> [1]
  | BLT -> [1]
  | BGE -> [1]
  | BGT -> [1]

  | JAL  -> [0]
  | JALR -> [0]
  | JR   -> [0]
  | JMP  -> [1]

  | _ -> assert false (* Cas du MOV *)

let opcode = function
  | ADD  -> [0;1;1]
  | SUB  -> [0;1;1]
  | OR   -> [0;1;1]
  | NAND -> [0;1;1]
  | XOR  -> [0;1;1]
  | NXOR -> [0;1;1]
  | AND  -> [0;1;1]
  | NOR  -> [0;1;1]
  | SLL  -> [0;1;1]
  | SRL  -> [0;1;1]
  | SRA  -> [0;1;1]
  | SEQ  -> [0;1;1]
  | SLT  -> [0;1;1]

  | ADDI  -> [0;0;1]
  | SUBI  -> [0;0;1]
  | ORI   -> [0;0;1]
  | NANDI -> [0;0;1]
  | XORI  -> [0;0;1]
  | NXORI -> [0;0;1]
  | ANDI  -> [0;0;1]
  | NORI  -> [0;0;1]
  | SLLI  -> [0;0;1]
  | SRLI  -> [0;0;1]
  | SRAI  -> [0;0;1]
  | SEQI  -> [0;0;1]
  | SLTI  -> [0;0;1]
  
  | LW  -> [0;0;0]
  | SW  -> [0;1;0]

  | BEQ -> [1;1;0]
  | BNE -> [1;1;0]
  | BLE -> [1;1;0]
  | BLT -> [1;1;0]
  | BGE -> [1;1;0]
  | BGT -> [1;1;0]

  | JAL  -> [1;1;1]
  | JALR -> [1;0;1]
  | JR   -> [1;0;0]
  | JMP  -> [1;1;0]

  | _ -> assert false (* Cas du MOV *)

let imm_to_bin n = 
  let l=ref([]) in 
  let d=ref(n) in 
  let r=ref(0) in
  for i=0 to 15 do
    r:=!d mod 2;
    d:=!d / 2;
    l:=(!r)::(!l);
  done;
  !l
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