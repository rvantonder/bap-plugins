open Bap.Std
open Options
open Trim

type t = {
  check : Check.t;
  path_dir: string;
  trim_dir : string;
  count : int;
  project : project;
  options: options;
  trim : trim;
  max_depth : int;
  g : (module Bap.Std.Graphlib.Graph with type edge = Graphlib.Tid.Tid.edge and
      type node = tid and type t = Graphlib.Tid.Tid.t)
}
