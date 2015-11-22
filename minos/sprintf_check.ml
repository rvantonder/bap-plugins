open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy

module P = Policy.Predicate

(** CHECK *)
let max_paths = 0

let should_produce' ctxt args sink_blk =
  Format.printf "In should produce\n%!";
  match args with
  | {arg2 = Some (tid,_,_)}
    when P.arg_is_not_string ctxt.project sink_blk tid ->
    Output.trim_priority ctxt.trim_dir 0;
    Output.misc (Format.sprintf "Producing X\n");
    false
  | {arg2 = Some (tid2,_,_)} -> (** arg must be a string *)
    let str =
      Policy.get_arg_as_string ctxt.project sink_blk tid2 |> Util.val_exn in
    Output.misc (Format.sprintf "Found sprintf argument \"%s\"\n" str);
    Output.trim_priority ctxt.trim_dir 1;
    false
  | _ ->
    Output.trim_priority ctxt.trim_dir 5;
    false

(** CHECK *)
let check_path' inter_dependence path_attrs sub_path sink_blk =
  match path_attrs with
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence &&
      Policy.dep_blk_span dep sub_path = 1
    -> 0
  | {args = {arg1 = Some (_,_,dep)}; jmp_deps; num_blks} when
      P.size_eq_0 inter_dependence
    -> 1
  | _ -> 5

let check_path sink_blk sub_path (ctxt : Check.ctxt) path_attrs =
  (** AUX DATA *)
  let inter_dependence,arg_deps = match path_attrs.args with
    | {arg1 = Some (_,_,arg_deps)} ->
      Dependence.inter_dep arg_deps path_attrs.jmp_deps,arg_deps
    | _ -> Seq.empty,Seq.empty in

  let jmp_tids = Check_tmpl.get_jmp_tids sub_path in
  (** Output *)
  Dependence.output sub_path arg_deps path_attrs.jmp_deps
    inter_dependence jmp_tids ctxt;

  check_path' inter_dependence path_attrs sub_path sink_blk

(** Infer arguments from sink blk, if sink blk is the sink tid. Then
    check the path. *)
let run ctxt =
  let sink_blk = Term.last blk_t ctxt.sub_path |> Util.val_exn in
  let args = Check_tmpl.infer_args_with_deps ctxt sink_blk in
  let jmp_deps = Check_tmpl.get_jmp_deps ctxt.sub_path in
  let num_blks = Term.enum blk_t ctxt.sub_path |> Seq.length in
  let path_attr = {args; jmp_deps; num_blks} in
  check_path sink_blk ctxt.sub_path ctxt path_attr

let should_produce ctxt =
  Format.printf "checking should produce\n%!";
  let open Trim in
  (** AUX DATA *)
  (* TODO hardcoding is bad. Also, negative with overflow *)
  let test =
    let trim = ctxt.trim in
    let sink_blk = Term.find blk_t ctxt.trim.trim_sub trim.sink_tid |>
                   Util.val_exn in
    let args = Check_tmpl.infer_args ctxt sink_blk in
    should_produce' ctxt args sink_blk in
  if ctxt.num_paths < max_paths && ctxt.num_paths > 0 && test then
    true
  else
    false

let check : (Check.t) =
  {should_produce; run;
   reverse=false; max_depth=(-1)}
