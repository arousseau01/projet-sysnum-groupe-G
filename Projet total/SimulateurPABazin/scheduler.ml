open Netlist_ast
open Graph

exception Combinational_cycle

let read_arg = function Avar x->[x]|Aconst k -> []

let read_exp eq = let x,e = eq in match e with
    | Earg a -> read_arg a
    | Ereg x -> []
    | Enot a -> read_arg a
    | Ebinop (bi,a,b) -> read_arg a @ read_arg b
    | Emux (a,b,c) -> read_arg a @ read_arg b @ read_arg c
    | Erom (n,m,c) -> read_arg c 
    | Eram (n,m,a,b,c,d) -> read_arg a
    | Econcat (c,d) -> read_arg c @ read_arg d 
    | Eslice (m,n,a) -> read_arg a 
    | Eselect (n,a) -> read_arg a 

let schedule p = 
  let g = Graph.mk_graph () in
  List.iter (fun (x,e) -> Graph.add_node g x) p.p_eqs;
  List.iter (fun (x,e) -> List.iter (fun y->if not(List.mem y p.p_inputs) then Graph.add_edge g y x) (read_exp (x,e))) p.p_eqs;
  try let ordre = Graph.topological g
  in  {p_eqs= List.map (fun x->x,List.assoc x p.p_eqs) ordre;
      p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars}
  with Graph.Cycle -> raise Combinational_cycle
