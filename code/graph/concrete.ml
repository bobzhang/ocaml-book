open Format

open Graph

module G = Imperative.Digraph.Concrete(struct
  type t = string
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end)
open G
let g = create ()

let _ = begin
  add_edge g  "a" "b";
  add_edge g  "b" "c";
  add_edge g  "c" "a";
end
module D = struct
  include G
  let vertex_name v = (V.label v)
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end

module Dot_ = Graphviz.Dot(D)

let _ =
  let chan = open_out "g.dot"in
  let fmt = formatter_of_out_channel chan in 
  let () = Dot_.fprint_graph fmt g  in
  let () = pp_print_flush fmt () in 
  close_out chan 
