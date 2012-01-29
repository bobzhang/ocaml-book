(** pretty useful *)
#default_quotation "expr";;

open Camlp4.PreCast
open Format

module FV = Camlp4.Struct.FreeVars.Make(Ast)
module S = FV.S

let _loc = Loc.ghost

let pervasives =
  let list = ["+";
	      "-";
	     "/";
	     "*";] in
  List.fold_right S.add list S.empty

(** don't understand yet *)    
let collect_free_vars_sets = object (self)
  inherit [S.t] FV.fold_free_vars
    S.add ~env_init:pervasives S.empty as super 
  val free_sets = [] 
  method set_free free =
    {< free = free >}
  method free_sets = free_sets
  method add_current_free =
    {< free_sets = free :: free_sets  >}
  method! expr = function
    | <<close_expr $e$ >> ->
      (self#expr e)#add_current_free#set_free free
    | e -> super#expr e
end 

let apply_close_expr next_free_set = object(self)
  inherit Ast.map as super
  method! expr = function
    | << close_expr $e$ >> ->
      let e = self#expr e in
      let fv = next_free_set () in
      S.fold (fun x acc -> << fun ~ $x$ -> $acc$ >>) fv e
    | e -> super#expr e
end
let f st =
  let fv_sets = ref (collect_free_vars_sets#str_item st)#free_sets in
  let next_free_set () = 
    match !fv_sets with
      | [] -> assert false
      | x:: xs ->
	let _ = fv_sets := xs in
	x
  in (apply_close_expr next_free_set)#str_item st

let _ =
  AstFilters.register_str_item_filter f 
