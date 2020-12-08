open Netlist_ast

type t

val create : program -> t
(*val flash_rom : t -> string -> unit*)
val set_value : t -> ident -> value -> unit
val get_value : t -> ident -> value
val get_type : t -> ident -> ty

val rom_size : t -> int * int
val ram_size : t -> int * int
val read_rom : t -> value -> value
val read_ram : t -> value -> value
val write_ram : t -> value -> value -> unit

