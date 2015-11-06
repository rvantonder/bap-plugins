open Bap.Std

type src_config =
  {src_at_root : bool;
   src_at_nth : int;
   src : string }

type cut_group = {
  (* we will do similarity testing with the actual blks. Stores
     the blks that call a given source *)
  src_caller_blks : Blk.t seq;
  sink_caller_blks : Blk.t seq;
  (* The callstring from src to the lca *)
  src_callstring : tid seq;
  sink_callstring : tid seq;
  (* The sub that calls src. src_caller_blks are all contained in
     this sub *)
  src_caller_sub : Sub.t;
  sink_caller_sub : Sub.t;
  lca_sub : Sub.t; (* root *)
  lca_name : string;
  depth: int;
  id: int; (* max depth we would have to inline to hit this *)
}


val print_cut_group : cut_group -> unit

val output_cut_group : cut_group -> unit

val cuts : project -> Graphlib.Callgraph.t -> src_config -> string -> cut_group seq
