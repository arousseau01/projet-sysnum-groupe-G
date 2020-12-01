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

let rec descendre t = match t.n_mark with
  |Visited-> ()
  |InProgress-> raise Cycle
  |NotVisited-> t.n_mark<-InProgress; has_cycle_list t.n_link_to; t.n_mark<-Visited;
and 
has_cycle_list l = match l with
  |[] -> ()
  |t::q -> descendre t;has_cycle_list q;;

let has_cycle g = try has_cycle_list g.g_nodes; false with Cycle -> true;;

let topological g = 
  let rep = ref([]) in
  let rec descendre_bis t = match t.n_mark with
    |NotVisited-> ()
    |Visited-> t.n_mark<-InProgress; visit_list t.n_link_to; t.n_mark<-NotVisited; rep:=(t.n_label::!rep);
    |_->()
  and 
  visit_list l = match l with
    |[] -> ()
    |t::q -> descendre_bis t;visit_list q;
  in visit_list g.g_nodes;
  !rep;;


  

