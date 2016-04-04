Three modules:

* `Bap_ir_ddg`
* `Bap_ir_cdg`
* `Bap_ir_pdg`

They are all of module type `Bap_ir_tid_graph`. `Bap_ir_tid_graph` uses the
same common `pp` printer for all graphs.

CDG can be control dependence of blocks (``Blk), or statements(``Stmt). For
statements, we create an edge from a jump to each `def`,`jmp`,and `phi` in the
target block (this seems to be the standard convention). Only `Stmt can be
sensibly unioned with DDG, which is only statements.

One question is what we can do for exit nodes, which is needed for the control
dependence algorithm. The best idea so far is to create a stub exit node, but I
have not done this yet.

`main.ml` is not so important, it is mostly a driver for graphs, and to create
`.dot` files with statements and not just tids.
