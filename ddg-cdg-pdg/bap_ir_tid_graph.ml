open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std

module GT = Graphlib.Make(Tid)(Unit)

include GT

include Printable(struct
    type t = GT.t
    let module_name = None
    let version = "0.1"

    let pp ppf g =
      Graphlib.Dot.pp_graph
        ~string_of_node:Tid.name
        ~attrs:["node[shape=box]"]
        ~nodes_of_edge:(fun e -> GT.Edge.(src e, dst e))
        ~nodes:(GT.nodes g)
        ~edges:(GT.edges g) ppf
  end)
