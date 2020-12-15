open Instr

type stream = {
  (* returns the current token *)
  peek : unit -> Lexer.token ;
  (* returns the current token and advances to the next token *)
  next : unit -> Lexer.token ;
}


let section = ref S_TEXT
let pos_text = ref 0
let pos_data = ref 0

(* size in bytes *)
let instr_size = 4

exception ParsingError of string
let error msg = raise (ParsingError msg)

let parse_section s =
  begin match s.peek () with
    | DOT_SECTION -> let _ = s.next () in ()
    | _ -> error "expected .section"
  end;
  match s.peek () with
    | DATA -> let _ = s.next () in section := S_DATA
    | TEXT -> let _ = s.next () in section := S_TEXT
    | _ -> error "expected a section name (text/data for instance)"

let parse_space s =
  begin match s.peek () with
    | DOT_SPACE -> let _ = s.next () in ()
    | _ -> error "expected .space"
  end;
  let space = match s.peek () with
    | INT i when i >= 0 -> let _ = s.next () in i
    | _ -> error "expected a non-negative number"
  in
  match !section with
    | S_DATA -> pos_data := !pos_data + space
    | S_TEXT -> pos_text := !pos_text + space
    | _ -> assert false

let parse_reg s = 
  match s.peek () with
    | REG r -> let _ = s.next () in r
    | _ -> error "expected a register"

let parse_ident s =
  match s.peek () with
    | IDENT i -> let _ = s.next () in i
    | _ -> error "expected an identifier"

let parse_label s =
  let name = parse_ident s in
  begin match s.peek () with
    | COLON -> let _ = s.next () in ()
    | _ -> error "expected a colon"
  end;
  let pos = match !section with
    | S_TEXT -> !pos_text
    | S_DATA -> !pos_data
    | _ -> assert false
  in
  { lname = name ; lpos = pos ; section = !section }

(* the lexer doesn't generate 'immediate' instruction names
 * (i.e. it outputs add but never addi) *)
let parse_instr s = 
  if !section <> S_TEXT
  then error (Printf.sprintf 
    "instructions should only appear in section %s" 
    (section_to_string S_TEXT));

  let name = match s.peek () with
    | INSTR i -> let _ = s.next () in i
    | _ -> error "expected an instruction name"
  in
  let instr = create_instr name in
  instr.ipos <- !pos_text; 
  pos_text := !pos_text + instr_size;
  begin match name with
    | n when is_reg_arith n -> 
      instr.rd <- parse_reg s;
      instr.rs1 <- parse_reg s;
      begin match s.peek () with
        | INT n -> 
          let _ = s.next () in
          instr.imm <- n;
          instr.iname <- imm_version name
        | REG r -> 
          let _ = s.next () in
          instr.rs2 <- r
        | _ -> error "expected a register/immediate"
      end;
    | n when is_branch n ->
      instr.rs1 <- parse_reg s;
      instr.rs2 <- parse_reg s;
      instr.label <- parse_ident s
    | MOV ->
      begin match s.peek () with
        (* store word *)
        | IND_REG r ->
          let _ = s.next () in
          instr.iname <- SW;
          instr.rs1 <- r;
          instr.rs2 <- parse_reg s;
          instr.imm <- 0
        (* store word *)
          | IDENT i ->
          let _ = s.next () in
          instr.iname <- SW;
          instr.rs1 <- RZ;
          instr.rs2 <- parse_reg s;
          instr.label <- i 
        | REG r1 ->
          let _ = s.next () in
          begin match s.peek () with
            (* add *)
            | REG r2 ->
              let _ = s.next () in
              instr.iname <- ADD;
              instr.rd <- r1;
              instr.rs1 <- r2;
              instr.rs2 <- RZ
            (* addi *)
            | INT n ->
              let _ = s.next () in 
              instr.iname <- ADDI;
              instr.rd <- r1;
              instr.rs1 <- RZ;
              instr.imm <- n
            (* load word *)
            | IND_REG r2 -> 
              let _ = s.next () in
              instr.iname <- LW;
              instr.rd <- r1;
              instr.rs1 <- r2;
              instr.imm <- 0
            (* load word *)
            | IDENT i ->
              let _ = s.next () in
              instr.iname <- LW;
              instr.rd <- r1;
              instr.rs1 <- RZ;
              instr.label <- i
            | _ -> error "expected a register/indirect register/label/immediate"
          end
        | _ -> error "expected a register/indirect register/label"
      end
    | _ -> failwith ("invalid instruction " ^ (instr_to_string name))
  end;
  instr

let parse_prog s =
  (* we need a section at the start of the file *)
  parse_section s;
  let rec loop i_list lab_list =
    match s.peek () with
      | EOF -> (List.rev i_list, List.rev lab_list)
      | IDENT _ ->
        let l = parse_label s in
        loop i_list (l :: lab_list)
      | DOT_SECTION -> 
        parse_section s; 
        loop i_list lab_list 
      | DOT_SPACE -> 
        parse_space s ; 
        loop i_list lab_list
      | _ ->
        let i = parse_instr s in
        loop (i :: i_list) lab_list
  in
  let i_list, lab_list = loop [] [] in
  (* fill in immediates for branch/load/store instructions,
   * and check the labels are unique and exist *)
  let labels = Hashtbl.create 8 in
  List.iter 
    (fun lab ->
      if Hashtbl.mem labels lab.lname 
      then error ("duplicate label " ^ lab.lname) 
      else Hashtbl.add labels lab.lname lab)
    lab_list;
  let fill_immediate i =
    if i.label <> "" then
    (* the section the label used in i should be defined in *) 
    let lab_section_opt = match i.iname with
      | _ when is_branch i.iname -> Some S_TEXT
      | LW | SW -> Some S_DATA
      | _ -> None
    in
    match lab_section_opt with
    | None -> ()
    | Some lab_section ->
      begin 
        try 
          let lab = Hashtbl.find labels i.label in
          if lab.section <> lab_section
          then error (Printf.sprintf 
            "label %s is defined in section %s but should be defined in section %s"
            lab.lname
            (section_to_string lab.section)
            (section_to_string lab_section))
          else i.imm <- lab.lpos 
        with Not_found ->
          error (Printf.sprintf 
            "couldn't find label %s" 
            i.label)
      end
  in
  List.iter fill_immediate i_list;
  (i_list, lab_list)
