
open Camlp4.PreCast;
  
value rec aux =
    let add_debug_expr e =
      let _loc = Loc.make_absolute (Ast.loc_of_expr e) in
      let msg = "Exception tracer at " ^
	Loc.to_string _loc ^ " (%s)@." in
      <:expr<
	try $e$
	with exc ->
	  do {
            Format.eprintf $str:msg$ (Printexc.to_string exc);
            raise exc
	  } >> in
    let map_pwe (patt, owhen, expr) =
      (patt, owhen, add_debug_expr expr) in
    map_pwe
and map_expr =
 fun
 [ <:expr@_loc< fun [ $ms$ ] >> ->
   let mcs = Ast.list_of_match_case ms [] in
   let mcs' = Ast.mcOr_of_list (List.map (fun [
   <:match_case< $p$ when $e1$ -> $e2$ >>
       ->
	 let (p',e1',e2') = aux (p,e1,e2) in
	 <:match_case<$p'$ when $e1'$ -> $e2'$ >>
   ]) mcs ) in
   <:expr< fun [ $mcs'$ ]>>
 | x -> x ];

  
(*
  value make_absolute x =
  debug loc "make_absolute: %a@\n" dump x in
  let pwd = Sys.getcwd () in
  if Filename.is_relative x.file_name then
    { (x) with file_name = Filename.concat pwd x.file_name }
  else x;
*)

AstFilters.register_str_item_filter (Ast.map_expr map_expr)#str_item  ;
