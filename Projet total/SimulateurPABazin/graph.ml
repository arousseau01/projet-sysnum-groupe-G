exception Cycle
type mark = NotVisited | InProgress | Visited

type 'a graph =
    { mutable g_nodes : 'a node list }
and 'a node = {
  n_label : 'a;
  mutable n_mark : mark;
  mutable n_link_to : 'a node list;
  mutable n_linked_by : 'a node list;
}

let mk_graph () = { g_nodes = [] }

let add_node g x =
  let n = { n_label = x; n_mark = NotVisited; n_link_to = []; n_linked_by = [] } in
  g.g_nodes <- n::g.g_nodes

let node_for_label g x =
  List.find (fun n -> n.n_label = x) g.g_nodes

let add_edge g id1 id2 =
  let n1 = node_for_label g id1 in
  let n2 = node_for_label g id2 in
  n1.n_link_to <- n2::n1.n_link_to;
  n2.n_linked_by <- n1::n2.n_linked_by

let clear_marks g =
  List.iter (fun n -> n.n_mark <- NotVisited) g.g_nodes

let find_roots g =
  List.filter (fun n -> n.n_linked_by = []) g.g_nodes

let rec has_cycle g = clear_marks g;aux2 g.g_nodes 
and aux2 = function 
  |[] -> false
  |n::q -> aux n || aux2 q 
and aux n = match n.n_mark with
  |InProgress -> true
  |Visited -> false
  |NotVisited -> n.n_mark <- InProgress; let r = aux2 n.n_link_to in n.n_mark<-Visited;r
 
let rec topological g = clear_marks g; if g.g_nodes=[] then [] else
  let f = find_roots g in 
  if f=[] then raise Cycle else
    let rec aux2b fin = function 
      |[] -> fin
      |n::q -> auxb (aux2b fin q) n 
    and auxb fin n = match n.n_mark with
      |InProgress -> raise Cycle
      |Visited -> fin
      |NotVisited -> n.n_mark <- InProgress; let r = aux2b fin n.n_link_to in n.n_mark<-Visited;n.n_label::r
  in aux2b [] f;;