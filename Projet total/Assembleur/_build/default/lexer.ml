# 1 "lexer.mll"
 
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

# 81 "lexer.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\246\255\247\255\001\000\077\000\160\000\237\000\056\001\
    \252\255\253\255\254\255\255\255\066\001\078\001\155\001\232\001\
    \058\002\248\255\125\000\253\255\254\255\255\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\008\000\008\000\005\000\004\000\008\000\
    \255\255\255\255\255\255\255\255\004\000\255\255\006\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\019\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\011\000\010\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \011\000\000\000\000\000\000\000\000\000\004\000\015\000\000\000\
    \003\000\000\000\000\000\000\000\000\000\007\000\005\000\000\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\008\000\009\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\014\000\000\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\021\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \000\000\000\000\000\000\000\000\014\000\000\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\000\000\000\000\000\000\000\000\005\000\
    \001\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\000\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \000\000\000\000\000\000\000\000\005\000\000\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\014\000\020\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\000\000\000\000\000\000\000\000\014\000\000\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\000\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\000\000\000\000\
    \000\000\000\000\014\000\000\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\016\000\000\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\000\000\000\000\000\000\000\000\016\000\
    \000\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\017\000\000\000\000\000\000\000\000\000\
    \016\000\000\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\000\000\000\000\000\000\
    \000\000\016\000\000\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\003\000\255\255\
    \000\000\255\255\255\255\255\255\255\255\000\000\000\000\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\255\255\255\255\255\255\000\000\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\004\000\255\255\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\018\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \255\255\255\255\255\255\255\255\004\000\255\255\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\005\000\255\255\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\255\255\255\255\255\255\255\255\005\000\
    \000\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\005\000\005\000\005\000\005\000\005\000\
    \005\000\005\000\005\000\006\000\255\255\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \255\255\255\255\255\255\255\255\006\000\255\255\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
    \007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
    \007\000\007\000\012\000\012\000\012\000\012\000\012\000\012\000\
    \012\000\012\000\012\000\012\000\013\000\018\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\255\255\255\255\255\255\255\255\013\000\255\255\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
    \013\000\014\000\255\255\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\255\255\255\255\
    \255\255\255\255\014\000\255\255\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\014\000\014\000\
    \014\000\014\000\014\000\014\000\014\000\014\000\015\000\255\255\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\255\255\255\255\255\255\255\255\015\000\
    \255\255\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\015\000\015\000\015\000\015\000\015\000\
    \015\000\015\000\015\000\016\000\255\255\255\255\255\255\255\255\
    \016\000\255\255\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\255\255\255\255\255\255\
    \255\255\016\000\255\255\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\016\000\016\000\016\000\
    \016\000\016\000\016\000\016\000\016\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec token lexbuf =
   __ocaml_lex_token_rec lexbuf 0
and __ocaml_lex_token_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 86 "lexer.mll"
               ( token lexbuf )
# 326 "lexer.ml"

  | 1 ->
# 87 "lexer.mll"
         ( new_line lexbuf; token lexbuf )
# 331 "lexer.ml"

  | 2 ->
# 88 "lexer.mll"
        ( comment lexbuf )
# 336 "lexer.ml"

  | 3 ->
# 89 "lexer.mll"
        ( COLON )
# 341 "lexer.ml"

  | 4 ->
let
# 90 "lexer.mll"
           n
# 347 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 90 "lexer.mll"
             ( INT (parse_int n) )
# 351 "lexer.ml"

  | 5 ->
let
# 91 "lexer.mll"
             i
# 357 "lexer.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 91 "lexer.mll"
               (
    try 
      let instr = Hashtbl.find instr_table i in
      INSTR instr
    with Not_found -> begin
      try Hashtbl.find keyword_table i 
      with Not_found ->
        IDENT i
    end
  )
# 370 "lexer.ml"

  | 6 ->
let
# 101 "lexer.mll"
                 i
# 376 "lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 1) lexbuf.Lexing.lex_curr_pos in
# 101 "lexer.mll"
                    (
    try 
      let reg = Hashtbl.find reg_table i in
      REG reg
    with Not_found ->
      error ("unknown register name " ^ i)
  )
# 386 "lexer.ml"

  | 7 ->
let
# 108 "lexer.mll"
                  i
# 392 "lexer.ml"
= Lexing.sub_lexeme lexbuf (lexbuf.Lexing.lex_start_pos + 2) (lexbuf.Lexing.lex_curr_pos + -1) in
# 108 "lexer.mll"
                        (
    try 
      let reg = Hashtbl.find reg_table i in
      IND_REG reg
    with Not_found ->
      error ("unknown register name " ^ i)
  )
# 402 "lexer.ml"

  | 8 ->
let
# 115 "lexer.mll"
         c
# 408 "lexer.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 115 "lexer.mll"
           ( 
    error (Printf.sprintf 
      "unexpected character %s" 
      (String.make 1 c))
  )
# 416 "lexer.ml"

  | 9 ->
# 120 "lexer.mll"
        ( EOF )
# 421 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_token_rec lexbuf __ocaml_lex_state

and comment lexbuf =
   __ocaml_lex_comment_rec lexbuf 18
and __ocaml_lex_comment_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 123 "lexer.mll"
         ( new_line lexbuf; token lexbuf )
# 433 "lexer.ml"

  | 1 ->
# 124 "lexer.mll"
        ( error "unexpected eof in comment" )
# 438 "lexer.ml"

  | 2 ->
# 125 "lexer.mll"
      ( comment lexbuf )
# 443 "lexer.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_comment_rec lexbuf __ocaml_lex_state

;;
