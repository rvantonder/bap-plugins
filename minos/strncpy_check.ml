open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy

module P = Policy.Predicate

let max_paths = 30000

(** CHECK : similar to memcpy. Produce if the third argument is symbolic *)
let should_produce' ctxt args sink_blk =
  match args with
  | {arg3 = Some (tid,_,_)} when
      P.arg_is_not_const sink_blk tid ->
    Output.trim_priority ctxt.trim_dir 0;
    true
  | _ -> false

(** CHECK : do not observe dependencies *)
let check_path' sub_path = 0

let check_path sink_blk sub_path (ctxt : Check.ctxt) =
  (** Output *)
  Dependence.output sub_path Seq.empty Seq.empty Seq.empty Seq.empty ctxt;
  check_path' sub_path

(** Infer arguments from sink blk, if sink blk is the sink tid. Then
    check the path. *)
let run ctxt =
  let sink_blk = Term.last blk_t ctxt.sub_path |> Util.val_exn in
  check_path sink_blk ctxt.sub_path ctxt

let should_produce ctxt =
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
  {should_produce; run}
