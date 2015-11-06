open Core_kernel.Std
open Bap.Std
open Check

let consume sub_path (check : Check.t) (ctxt : Check.ctxt) =
  (** Priority type may change, so stick with pattern matching *)
  match check.run ctxt with
  | p -> Output.path_priority ctxt.path_dir ctxt.count p
