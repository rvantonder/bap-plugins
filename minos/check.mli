open Bap.Std
open Options
open Trim

(* This is a ctxt type for check *)
type ctxt = {
    sub_path : Sub.t;
    num_paths : int;
    path_dir : string;
    options : options;
    project : project;
    trim : trim;
    trim_dir : string;
    count : int;
}

type t = {
  should_produce : (ctxt -> bool);
  run: (ctxt -> int);
  reverse : bool;
  max_depth : int;
  sample : int;
  timeout : int
}
