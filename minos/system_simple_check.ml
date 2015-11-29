open Bap.Std
open Core_kernel.Std
open Options
open Args
open Check
open Check_tmpl
open Policy
open Format

module P = Policy.Predicate

(** This is before paths. We will fold const on the sub with
    respect to system *)
let should_produce' ctxt args sink_blk =
  match args with
  | {arg1 = Some (tid,_,_)}
    when P.arg_is_not_string ctxt.project sink_blk tid ->
    Output.trim_priority ctxt.trim_dir 0;
    Output.misc (Format.sprintf "Producing for symbolic\n");
    true
  | {arg1 = Some (tid,_,_)} -> (** Arg must be a string *)
    let str =
      Policy.get_arg_as_string ctxt.project sink_blk tid |> Util.val_exn in
    if String.is_substring ~substring:"%" str then (
      Output.misc (Format.sprintf
                     "Found system argument with format specifier %S\n" str);
      Output.trim_priority ctxt.trim_dir 0)
    else (
      Output.misc (Format.sprintf "Found system argument %S\n" str);
      Output.trim_priority ctxt.trim_dir 1);
    false
  | _ ->
    Output.trim_priority ctxt.trim_dir 5;
    false

(** helper function to resolve constant string arguments to sprintf/snprintf *)
let printf_arg ctxt sub =
  (** get the blk closest at the end of the path that calls sprintf or snprintf *)
  let blks =
    Term.enum blk_t sub |> Seq.fold ~init:Seq.empty ~f:(fun acc blk ->
        if List.exists (Util.calls_of_blk_str blk) ~f:(fun s ->
            s = "@sprintf" || s = "@snprintf") then
          (blk ^:: acc) else acc) in
  match Seq.hd blks with
  | Some blk ->
    (** Infer args *)
    (** return the string it contains *)
    let args = Check_tmpl.infer_args ctxt blk in
    (match args with
     | {arg2 = Some (tid,_,_)}
       when P.arg_is_string ctxt.project blk tid ->
       Policy.get_arg_as_string ctxt.project blk tid
     | {arg3 = Some (tid,_,_)}
       when P.arg_is_string ctxt.project blk tid ->
       Policy.get_arg_as_string ctxt.project blk tid
     | _ -> None)
  | None -> None

(** CHECK *)
(** Path contains a system with symbolic argument. Check if
    it is preceded by a call to sprintf or snprintf *)
let check_path' ctxt path_attrs sub_path sink_blk =
  match path_attrs with
  | {args = {arg1 = Some _}; _} when
      P.contains_calls ["@sprintf"; "@snprintf"] sub_path
    -> (match printf_arg ctxt sub_path with
        | Some s -> Output.misc (Format.sprintf "s[n]printf arg: %S\n" s); 5
        | None -> 5)
  | _ -> 0

let check_path sink_blk sub_path (ctxt : Check.ctxt) path_attrs =
  check_path' ctxt path_attrs sub_path sink_blk

(** Infer arguments from sink blk, if sink blk is the sink tid. Then
    check the path. *)
let run ctxt =
  let sink_blk = Term.last blk_t ctxt.sub_path |> Util.val_exn in
  let args = Check_tmpl.infer_args ctxt sink_blk in
  let path_attr = {args; jmp_deps = Seq.empty; num_blks = 0} in
  check_path sink_blk ctxt.sub_path ctxt path_attr

let should_produce ctxt =
  let open Trim in
  (** AUX DATA *)

  let test =
    (** Get sink blk after performing SSA *)
    let sink_blk = Term.find blk_t ctxt.trim.trim_sub ctxt.trim.sink_tid |>
                   Util.val_exn in

    let args = Check_tmpl.infer_args ctxt sink_blk in
    should_produce' ctxt args sink_blk in

  (** Also produces even if there are many paths, because we sample *)
  if test then
    true
  else
    false

let check : (Check.t) =
  let max_depth = 100 in
  let sample = 10 in  (** contrast with 1000 :3 *)
  let timeout = 3 in
  let reverse = true in
  {should_produce; run;
   reverse;
   max_depth;
   sample;
   timeout}
