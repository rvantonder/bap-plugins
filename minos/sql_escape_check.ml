open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy

module P = Policy.Predicate

(** CHECK *)
let max_paths = 30000

let should_produce' () =
  true

(** CHECK : Is there an escape on this path at all? *)
let check_path' sub_path =
  let p = Policy.Predicate.contains_calls in
  let pred = p ["@_ZN7OpenDBX4Conn6escapeERKSsRSs";
                "@sub_11E18"; "@sub_18324"] sub_path in
  match pred with
  | false -> 0
  | true -> 5

let check_path sink_blk sub_path (ctxt : Check.ctxt) =
  (** AUX DATA *)
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
  let test = should_produce' () in
  if ctxt.num_paths < max_paths && ctxt.num_paths > 0 && test then
    true
  else
    false

let check : (Check.t) =
  {should_produce; run}
