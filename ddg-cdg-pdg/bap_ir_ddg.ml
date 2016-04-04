open Core_kernel.Std
open Regular.Std
open Graphlib.Std
open Bap.Std

module G = Bap_ir_tid_graph

module type Vars = sig
  type t
  val free_vars : t -> Var.Set.t
end

let lookup sub v =
  with_return_option (fun {return} ->
      (object
        inherit [unit] Term.visitor
        method! enter_phi phi () = if Phi.lhs phi = v then return (Term.tid phi)
        method! enter_def def () = if Def.lhs def = v then return (Term.tid def)
      end)#visit_sub sub ())

(** get definition tids of free_vars *)
let defs sub free_vars =
  let exps = List.map free_vars ~f:(fun v -> lookup sub v) in
  List.concat_map exps ~f:Option.to_list

let add sub g =
  let add_edge elt g dd_elt =
    let edge = G.Edge.create elt dd_elt () in
    G.Edge.insert edge g in
  let add_deps (type t) tid (module M : Vars with type t = t) elt =
    let deps = defs sub (Set.to_list (M.free_vars elt)) in
    List.fold deps ~init:g ~f:(add_edge (tid elt)) in
  function
  | `Def def -> add_deps Term.tid (module Def) def
  | `Phi phi -> add_deps Term.tid (module Phi) phi
  | `Jmp jmp -> add_deps Term.tid (module Jmp) jmp

let create sub =
  let sub = Sub.ssa sub in
  Term.enum blk_t sub |> Seq.fold ~init:G.empty ~f:(fun g blk ->
      Blk.elts blk |> Seq.fold ~init:g ~f:(fun g elt ->
          add sub g elt))

include G
