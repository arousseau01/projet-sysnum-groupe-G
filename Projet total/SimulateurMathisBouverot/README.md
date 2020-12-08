Netlist Simulator by Mathis Bouverot-Dupuis

To build the simulator, use :
ocamlbuild netlist_simulator.byte
To run the simulator on netlist file.net for i steps, use :
./netlist_simulator.byte -n i file.net
Some examples are included in folder Test.

Project structure :
The entry point is netlist_simulator.ml.
The simulator uses a mutable context to store values of identifiers and memory state, defined in files context.ml/context.mli.
The simulator supports up to one ROM and one RAM, the behaviour of which is defined in module Memory of context.ml.
Input/output is done using file io.ml/io.mli.
Reading a netlist is done using files netlist.ml/netlist_lexer.mll/netlist_parser.mly: these files were left untouched.
Scheduling the netlist equations is done with files scheduler.ml/graph.ml.

As of now the ROM is useless : it is always 0. In the future (when I understand more about what are the requirements for the simulator, i.e. what functionalities it must support) I will probably add support for initializing it from a file.

I also plan on rewriting the entire simulator (netlist parser included) in C or C++, as I have experienced Ocaml's functional style to be horrible for this project. Again I will assess this again later, when I'll have progressed in the project.
