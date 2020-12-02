open Netlist_ast


val print_greetings : Context.t -> unit
val read_inputs : Context.t -> int -> ident list -> unit
val print_outputs : Context.t -> int -> ident list -> unit
