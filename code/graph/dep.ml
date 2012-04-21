open Format
open Graph 
module V = struct
  type t = string
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end
module StringDigraph = Imperative.Digraph.Concrete (V)
open StringDigraph
module Display = struct 
  include StringDigraph
  open StringDigraph
  let vertex_name v = (V.label v)
  let graph_attributes _ = []
  let default_vertex_attributes _ = []
  let vertex_attributes _ = []
  let default_edge_attributes _ = []
  let edge_attributes _ = []
  let get_subgraph _ = None
end 
open StringDigraph
module D = Graphviz.Dot(Display)
let finally handler f x =
  let r = (
    try
      f x
    with
      e -> handler(); raise e
   ) in
  handler();
  r

let dot_output g  =
   let () =  D.fprint_graph std_formatter g in
   pp_print_flush std_formatter ()

let test_line = "path.ml: Hashtbl Heap List Queue Sig Util"
open Camlp4.PreCast
let parser_of_entry entry  s =
  try Gram.parse entry (Loc.mk "<string>") (Stream.of_string  s)
  with
    Loc.Exc_located(loc, e) -> begin 
      prerr_endline (Loc.to_string loc);
      let start_bol,stop_bol,
        start_off, stop_off =
        Loc.(start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc
            ) in
      let abs_start_off = start_bol + start_off in
      let abs_stop_off = stop_bol + stop_off in
      let err_location = String.sub s abs_start_off
          (abs_stop_off - abs_start_off + 1) in
      prerr_endline (sprintf "err: ^%s^" err_location);
      raise e ;
    end
        
        
let path_line = Gram.Entry.mk "path_line"
let path_line_eoi = Gram.Entry.mk "path_line_eoi"
    
let _ = begin
  EXTEND Gram GLOBAL: path_line path_line_eoi;
  path_line_eoi:
    [ [x = path_line ; `EOI -> x ] ];
  path_line:
    [
     "top"
       [ name=LIDENT;"."; ext=LIDENT;
         ":"; modules = LIST0 [x=UIDENT->x] ->
         (name,ext,modules)] ];

  END;
end


let parse_path_line = parser_of_entry path_line
let parse_path_line_eoi = parser_of_entry path_line_eoi

let string_map f s =
  let open String in
  let l = length s in
  if l = 0 then s else begin
    let r = create l in
    for i = 0 to l - 1 do unsafe_set r i (f(unsafe_get s i)) done;
    r
  end


let lowercase s = string_map Char.lowercase s
    
let _ =
  let g = create () in 
  try
    while true do
      let line = input_line stdin in 
      let (name,ext,deps) = parse_path_line_eoi line in
      List.iter (fun dep ->
        add_edge g (name^"_module") (lowercase dep ^ "_module")) deps 
    done
  with End_of_file -> begin
    prerr_endline "writing to dump.dot"; 
    dot_output g ;
    prerr_endline "finished";
  end
      




















