open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std

module G = Bap_ir_tid_graph

let create sub =
  let sub = Sub.ssa sub in
  let module CDG = Bap_ir_cdg in
  let module DDG = Bap_ir_ddg in
  let cdg = CDG.create `Stmt sub in
  let ddg = DDG.create sub in
  Graphlib.union (module Bap_ir_tid_graph) cdg ddg

include G
