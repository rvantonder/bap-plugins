open Core_kernel.Std
open Bap.Std
open Graphlib.Std
open Format
open Option
include Self()

type options = {fname : string option}

module Cmdline = struct
  include Cmdliner

  let fname : string option Term.t =
    let doc = "only one function" in
    Arg.(value & opt (some string) None & info ["fname"] ~doc)

  let process_args fname = {fname}

  let info =
    Term.info
      ~doc:""
      "Graph driver: print the data dependence graph (DDG), control
    dependence graph (CDG), and program dependence graph (PDG) in dot format."

  let parse argv =
    let args = Term.(pure process_args $fname) in
    match Term.eval ~argv (args,info) with
    | `Ok res -> res
    | `Error err -> exit 1
    | `Version | `Help -> exit 0
end

let left_justify =
  String.concat_map ~f:(fun c ->
      if c = '\n' then "\\l" else Char.to_string c)

let elt_of_tid sub tid : Blk.elt option =
  with_return_option (fun {return} ->
      (object
        inherit [unit] Term.visitor
        method! enter_phi phi () = if Term.tid phi = tid then return (`Phi phi)
        method! enter_def def () = if Term.tid def = tid then return (`Def def)
        method! enter_jmp jmp () = if Term.tid jmp = tid then return (`Jmp jmp)
      end)#visit_sub sub ())

let elt_dot ~filename sub g =
  let string_of_node node =
    match elt_of_tid sub node with
    | Some (`Phi phi) -> sprintf "\"\\%s\"" @@ Phi.to_string phi |> left_justify
    | Some (`Def def) -> sprintf "\"\\%s\"" @@ Def.to_string def |> left_justify
    | Some (`Jmp jmp) -> sprintf "\"\\%s\"" @@ Jmp.to_string jmp |> left_justify
    | None -> sprintf "\"\\%s\"" @@ Tid.name node |> left_justify in
  let node_attrs _ = [`Shape `Box] in
  Graphlib.to_dot (module Bap_ir_tid_graph) ~node_attrs ~string_of_node
    ~filename g

(** Print each graph for [sub]. Note to self: printf "%a\n" PDG.pp
    is also available *)
(** Note: if we SSA sub here, it will be SSA in the dot output. Note
    PDG will always use SSA form, so using SSA here is just for dot
    purposes. *)
let print sub =
  let fname = Sub.name sub in
  let sub = Sub.ssa sub in
  let module DDG = Bap_ir_ddg in
  let module CDG = Bap_ir_cdg in
  let module PDG = Bap_ir_pdg in
  let ddg = DDG.create sub in
  let cdg = CDG.create `Stmt sub in
  let pdg = PDG.create sub in
  elt_dot ~filename:("ddg_"^fname^".dot") sub ddg;
  elt_dot ~filename:("cdg_"^fname^".dot") sub cdg;
  elt_dot ~filename:("pdg_"^fname^".dot") sub pdg

let one_func program fname =
  let sub = Program.lookup sub_t program
      (Tid.from_string_exn ("@"^fname)) |> Option.value_exn in
  print sub

let all_funcs program =
  Term.enum sub_t program |> Seq.iter ~f:print

let main proj =
  let options = Cmdline.parse argv in
  let program = Project.program proj in
  match options.fname with
  | Some fname -> one_func program fname
  | None -> all_funcs program

let () = Project.register_pass' main
