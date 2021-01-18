let file = ref ""
let debug = ref false
let rom_file = ref ""

(*let correct_format f =
  let r = Str.regexp ".+\\.ms$" in
  Str.string_match r f 0*)

let anon_param f =
  if !file <> "" then
    raise (Arg.Bad "expected a single minias file")
  else if not (Filename.check_suffix f ".ms") then
    raise (Arg.Bad "expected a minias file (extension .ms)")
  else if not (Sys.file_exists f) then
    raise (Arg.Bad ("no such file : " ^ f))
  else
    file := f

let specs = [
  ("--debug", Arg.Set debug, ": print debugging info");
  ("--write-rom", Arg.Set_string rom_file, ": write the ROM contents to a file")
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
    if !debug then (Instr.print_prog p; print_newline ());
    let i_list, _ = p in
    let p = Codegen.codegen_prog i_list in
    if !debug then Codegen.print_bin_prog p;
    if !rom_file <> "" then Codegen.write_to_file !rom_file p
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

let _ = compile ()