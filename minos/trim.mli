open Bap.Std
open Cut

type trim = {
  trim_sub : Sub.t;
  cut_group : cut_group;
  src_tid : tid;
  sink_tid : tid
}

(* 'a is debug. 'b is highlight. *)
(* A given cut_group returns a sequence of trims (trim group) *)
val trims : 'a -> sub term -> cut_group -> 'b -> trim seq
