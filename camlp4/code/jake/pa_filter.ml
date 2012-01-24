
(* open BatPervasives; *)

open Camlp4.PreCast;
open Printf;
(* open Batteries; *)
(* value pim = Printers.OCaml.print_implem ; *)
open Util;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  value code_of_con_names name cons _loc =
    let match_cases = cons
      |> List.map (fun str -> <:match_case< $uid:str$ -> $str:str$ >>)
      (* |> Ast.mcOr_of_list *)
    in
    let reverse_cases = cons
      |> List.map (fun con -> <:match_case< $str:con$ -> $uid:con$ >>)
      (* |> Ast.mcOr_of_list *)
    in
    <:str_item<
      value $lid:(name^"_to_string") $ =
	  fun [ $list:match_cases$ ]
      ;
      value $lid:(name^"_of_string") $ =
	  fun [ $list:reverse_cases$ | x -> invalid_arg x ]
      ; >>;


 (** idea, view patterns are fit here, try it later *)	    
 value rec filter str_item = match str_item with
     [
       <:str_item@_loc< type $lid:tid$ = [ $t$ ]>> -> begin
	 try
	   let ctys = Ast.list_of_ctyp t [] in
	   let con_names =
	     List.map (fun [ <:ctyp< $uid:c$>> -> c
			   | x -> raise Not_found ]) ctys in
	   let code = code_of_con_names tid con_names _loc in
	   begin 
	     prerr_string "generating code right now";
	     <:str_item< $str_item$; $code$>>
	   end 
	 with
	     [exn -> begin
	       prerr_string (sprintf "%s\n : error \n" tid);
	       raise Not_found;
	     end ]
       end
     | x -> begin
       x
     end 
     ] ;
   AstFilters.register_str_item_filter filter;    
end ;

module Id = struct
  value name = "pa_filter";
  value version = "0.1";
end ;

value _ =
  let module M = Camlp4.Register.AstFilter Id Make in
  ();
