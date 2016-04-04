open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std

type kind = [`Blk | `Stmt]

module G = Bap_ir_tid_graph

let tid_of_node = Fn.compose Term.tid Graphs.Ir.Node.label
let blk_of_node = Graphs.Ir.Node.label
let (!) = tid_of_node
let (!!) = blk_of_node

let postdom_tree sub cfg =
  let start =
    Term.last blk_t sub (* TODO : assumption is this is an exit node *)
    |> Option.value_exn
    |> Graphs.Ir.Node.create in
  Graphlib.dominators (module Graphs.Ir) ~rev:true cfg start

let ipdom tree node = Tree.parent tree node

(** Collect all the nodes starting at [start_node] until we hit [ipdom]*)
let rec cds_of_node start_node ipdom tree acc =
  let (!) = Fn.compose Term.tid Graphs.Ir.Node.label in
  if !start_node = !ipdom then acc else
    let next = Tree.parent tree start_node in
    match next with
    | Some parent -> cds_of_node parent ipdom tree (start_node :: acc)
    | None -> acc

let to_tid = function
  | `Def def -> Term.tid def
  | `Phi phi -> Term.tid phi
  | `Jmp jmp -> Term.tid jmp

(** Find the matching jmp in [src_blk] for the [dst_block] *)
let add_stmt_edges g src_blk dst_blk =
  let open Option in
  (Term.enum jmp_t src_blk |> Seq.find_map ~f:(fun jmp ->
       match Jmp.kind jmp with
       | Goto (Direct tid) when tid = (Term.tid dst_blk)
         -> Some (Term.tid jmp)
       | _ -> None) >>= fun src_tid ->
   Seq.fold ~init:g (Blk.elts dst_blk) ~f:(fun g term ->
       let edge = G.Edge.create src_tid (to_tid term) () in
       G.Edge.insert edge g) |> return) |>
  Option.value ~default:g

(** Statement control dependences *)
let stmt_edges g src_node cds =
  let g' =
    List.fold cds ~init:g ~f:(fun g cd ->
        add_stmt_edges g !!src_node !!cd) in
  (* Add statements of nodes with no control dependences *)
  Blk.elts !!src_node |> Seq.fold ~init:g' ~f:(fun g'' term ->
      G.Node.insert (to_tid term) g'')

(** Block control dependences *)
let blk_edges g src_node cds =
  List.fold cds ~init:g ~f:(fun g cd ->
      let edge = G.Edge.create !src_node !cd () in
      G.Edge.insert edge g)
  (* Add blocks with no control dependences *)
  |> G.Node.insert !src_node

let create kind sub =
  let cfg = Sub.to_cfg sub in
  let tree = postdom_tree sub cfg in
  Seq.fold ~init:G.empty (Graphs.Ir.nodes cfg) ~f:(fun g node ->
      let succs = Graphs.Ir.Node.succs node cfg in
      Seq.fold succs ~init:[] ~f:(fun acc succ ->
          ipdom tree node |> function
          | Some ipdom ->
            cds_of_node succ ipdom tree acc
          | None -> acc) |> fun cds ->
      match kind with
      | `Blk -> blk_edges g node cds
      | `Stmt -> stmt_edges g node cds)

include G
