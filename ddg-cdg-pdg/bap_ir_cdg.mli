open Graphlib.Std
open Bap.Std

include module type of Bap_ir_tid_graph

type kind = [`Blk | `Stmt]

val create : kind -> sub term -> t
val pp : Format.formatter -> t -> unit
