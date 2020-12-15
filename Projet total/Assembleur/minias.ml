let file = ref ""
let debug = ref false

let correct_format f =
  let r = Str.regexp ".+\\.ms$" in
  Str.string_match r f 0

let anon_param f =
  if !file <> "" then
    raise (Arg.Bad "expected a single minias file")
  else if not (correct_format f) then
    raise (Arg.Bad "expected a minias file (extension .ms)")
  else if not (Sys.file_exists f) then
    raise (Arg.Bad ("no such file : " ^ f))
  else
    file := f

let specs = [
  ("--debug", Arg.Set debug, ": print debugging info")
]

let usage =
  Printf.sprintf "usage: %s <file>" Sys.argv.(0)

let create_stream lexbuf =
  let t = ref Lexer.EOF in
  let next () =
    let curr_t = !t in
    begin
      try t := Lexer.token lexbuf
      with Lexer.LexingError msg ->
        Format.eprintf 
          "File \"%s\", line %d, characters %d-%d:\nsyntax error: %s@."
          !file
          lexbuf.lex_start_p.pos_lnum
          (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1)
          (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
          msg;
        exit 1
    end;
    curr_t
  in
  let peek () = !t in
  let _ = next () in
  let open Parser in
  { next = next ; peek = peek }


let compile () =
  Arg.parse specs anon_param usage;
  if !file = "" then Arg.usage specs usage;
  let c = open_in !file in
  let lexbuf = Lexing.from_channel c in
  let s = create_stream lexbuf in
  try
    let p = Parser.parse_prog s in
    if !debug then Instr.print_prog p
  with 
    | Parser.ParsingError msg ->
      Format.eprintf 
        "File \"%s\", line %d, characters %d-%d:\nsyntax error: %s@."
        !file
        lexbuf.lex_start_p.pos_lnum
        (lexbuf.lex_start_p.pos_cnum - lexbuf.lex_start_p.pos_bol + 1)
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_start_p.pos_bol)
        msg;
      exit 1
    | _ -> 
      Format.eprintf "assembler internal error\n";
      exit 2

(*let _ = compile ()*)

let _ =
  let zero_4 = [0; 0; 0; 0] in
  let zero_8 = zero_4 @ zero_4 in
  let i = zero_8 @ zero_8 @ [1; 0; 1; 0] @ zero_4 @ zero_8 in
  let prog = [i; i; i] in
  Codegen.print_bin_prog prog