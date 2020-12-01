open Netlist_ast
open Graph

exception Combinational_cycle

let read_arg a = match a with 
|Avar x -> [x]
|_ -> []

let read_exp eq = let (x,e)=eq in match e with 
    | Earg a -> read_arg a
    | Ereg i -> []
    | Enot a -> read_arg a
    | Ebinop (_ , a1 , a2) -> read_arg a1 @ read_arg a2
    | Emux (a1 , a2 , a3) -> read_arg a1 @ read_arg a2 @ read_arg a3
    | Erom (_ , _, a) -> read_arg a
    | Eram (_ , _ , a ,_ ,_ ,_ ) -> read_arg a
    | Econcat (a1 , a2) -> read_arg a1 @ read_arg a2
    | Eslice (_ , _ , a) -> read_arg a
    | Eselect (_ , a) -> read_arg a;;

let print_graph g = let rec aux l = match l with 
|[]->()
|t::q-> print_string t.n_label; aux q
in aux g.g_nodes

let chercher_sommet x y g = let n=node_for_label g x in let rec aux l = match l with
|[]-> false
|t::q -> if t.n_label =y then true else aux q
in aux n.n_link_to

let rec print_list l = match l with
    |[]->()
    |t::q-> let (x,e)=t in print_string x; print_list q

    let rec print_list1 l = match l with
    |[]->()
    |t::q-> print_string t; print_list1 q

let schedule p = let g = mk_graph () in
    let rec vider l = match l with
    |[]->()
    |x::q-> add_node g x; vider q;

    in vider p.p_inputs;

    let rec vider1 l = match l with
    |[]->()
    |x::q-> add_node g (let (a,_)=x in a); vider1 q;

    in vider1 p.p_eqs;

    let rec aretes_list l y = match l with
    |[]->()
    |x::q -> if not (chercher_sommet x y g) then add_edge g x y ; aretes_list q y;

    in

    let rec aretes l = match l with
    |[] -> ()
    |eq::q -> let (x,e)=eq in aretes_list (read_exp eq) x; aretes q;

    in aretes p.p_eqs;

    if  (has_cycle g) then raise Combinational_cycle;

    let ordre = topological g in

    let rec list_eq leq ordre = match ordre with 
    |[]->List.rev leq
    |x::q -> try let eq= (List.find (fun n -> let (a,_)=n in a = x) p.p_eqs) in list_eq (eq::leq) q 
             with Not_found -> list_eq leq q;

    in

    let p1 = {p_eqs = list_eq [] ordre; p_inputs = p.p_inputs; p_outputs = p.p_outputs; p_vars = p.p_vars} in p1;;


