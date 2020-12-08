open Netlist_ast
open Graph

exception Combinational_cycle

module SS = Set.Make(String)
module SM = Map.Make(String)

let read_exp eq =
  let _, exp = eq in
  let vars = ref [] in
  let extract = function
    | Avar (x) -> vars := x :: !vars
    | _ -> ()
  in
  begin match exp with
    | Earg (a) -> extract a	
    | Ebinop (_, a1, a2) -> (extract a1; extract a2)
    | Enot (a) -> extract a
    | Emux (a1, a2, a3) -> (extract a1; extract a2; extract a3)
    | Erom (_, _, raddr) -> extract raddr
    | Eram (_, _, raddr, _, _, _) -> extract raddr
    | Ereg _ -> ()
    | Econcat (a1, a2) -> (extract a1; extract a2)
    | Eselect (i, a) -> extract a
    | Eslice (i, j, a) -> extract a
  end;
  !vars

let all_variables p = 
  let vars = ref SS.empty in
  let process_eq eq =
	let (x, _) = eq in
	vars := SS.add x !vars;
	List.iter (fun y -> vars := SS.add y !vars) (read_exp eq)
  in
  List.iter process_eq p.p_eqs;
	SS.elements !vars
 
let build_graph p =
  let g = mk_graph () in
  let vars = all_variables p in
  List.iter (add_node g) vars;
  let add_edges (x, exp) = 
	 let vars = read_exp (x, exp) in
	 List.iter (fun  y -> add_edge g y x) vars
  in
  List.iter add_edges p.p_eqs;
  g

let separate_reg_eqs eqs =
  let is_reg = function
	  | _, Ereg _ -> true
	  | _ -> false
  in
  let is_not_reg eq = not (is_reg eq)
  in
	List.filter is_reg eqs,
	List.filter is_not_reg eqs

let schedule p = 
  let g = build_graph p in 
  if has_cycle g then raise Combinational_cycle else
  let order = topological g in
  let n = List.length order in
  (* maps a variable to its position in order *)  
  let position = ref SM.empty in   
  List.iteri (fun i x -> position := SM.add x i !position) order;
  (* buckets.(i) will contain all the
   * equations that assign to the variable order.(i) *)
  let buckets = Array.make n [] in  
  let order_eq (x, exp) =
    let i = SM.find x !position in
	buckets.(i) <- (x, exp) :: buckets.(i)
  in
  List.iter order_eq p.p_eqs;
  let new_eqs = List.flatten (Array.to_list buckets) in
  (* place all the registers at the end of the instructions *)
  let reg, not_reg = separate_reg_eqs new_eqs in
    { 
      p_eqs = not_reg @ reg ; 
      p_inputs = p.p_inputs ; 
      p_outputs = p.p_outputs ;
      p_vars = p.p_vars 
    }

 
   

