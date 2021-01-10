{
  open Lexing
  open Instr

  exception LexingError of string
  let error msg = raise (LexingError msg)

  type token =
    | COLON
    | INT of int
    (* label names, etc. *)
    | IDENT of string
    | INSTR of instr_name
    (* %rax *)
    | REG of reg
    (* (%rax) *)
    | IND_REG of reg
    | DOT_SECTION | DOT_SPACE | DATA | TEXT
    | EOF

  (* instructions lw, sw, addi, subi, etc. 
   * are not visible to the programmer *)
  let instr_table = Hashtbl.create 64
  let _ = List.iter 
    (fun (name, i) -> Hashtbl.add instr_table name i)
    [("add", ADD);
    ("sub", SUB);
    ("or", OR);
    ("nand", NAND);
    ("xor", XOR);
    ("nxor", NXOR);
    ("and", AND);
    ("nor", NOR);
    ("sll", SLL);
    ("srl", SRL);
    ("sra", SRA);
    ("seq", SEQ);
    ("slt", SLT);

    ("mov", MOV);

    ("beq", BEQ);
    ("bne", BNE);
    ("ble", BLE);
    ("blt", BLT);
    ("bge", BGE);
    ("bgt", BGT);

    ("jal", JAL);
    ("jmp", JMP);
    
    ("print", PRINT)]

  (* register %rz is not visible to the programmer *)
  let reg_table = Hashtbl.create 8
  let _ = List.iter 
    (fun (name, r) -> Hashtbl.add reg_table name r)
    [("rax", RAX);
    ("rbx", RBX);
    ("rcx", RCX);
    ("rdx", RDX);
    ("rex", REX);
    ("rfx", RFX);
    ("rgx", RGX)] 

  let keyword_table = Hashtbl.create 4
  let _ = List.iter
    (fun (name, k) -> Hashtbl.add keyword_table name k)
    [(".section", DOT_SECTION);
    (".space", DOT_SPACE);
    ("data", DATA);
    ("text", TEXT)]

  let parse_int str =
    let n = int_of_string str in
    if n>=0 then if n<=32767 then n else error "only 16-bit integers supported"
    else if n>= -32768 then 65536+n else error "only 16-bit integers supported"
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_' '.']
let num = '-'?digit+
let ident = (alpha|digit)*alpha(alpha|digit)*

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '\n' { new_line lexbuf; token lexbuf }
  | ';' { comment lexbuf }
  | ':' { COLON }  
  | num as n { INT (parse_int n) }
  | ident as i {
    try 
      let instr = Hashtbl.find instr_table i in
      INSTR instr
    with Not_found -> begin
      try Hashtbl.find keyword_table i 
      with Not_found ->
        IDENT i
    end
  }
  | '%'(ident as i) {
    try 
      let reg = Hashtbl.find reg_table i in
      REG reg
    with Not_found ->
      error ("unknown register name " ^ i)
  }
  | "(%"(ident as i)')' {
    try 
      let reg = Hashtbl.find reg_table i in
      IND_REG reg
    with Not_found ->
      error ("unknown register name " ^ i)
  }
  | _ as c { 
    error (Printf.sprintf 
      "unexpected character %s" 
      (String.make 1 c))
  }
  | eof { EOF }

and comment = parse
  | '\n' { new_line lexbuf; token lexbuf }
  | eof { error "unexpected eof in comment" }
  | _ { comment lexbuf }