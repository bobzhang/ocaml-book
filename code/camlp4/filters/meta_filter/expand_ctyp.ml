(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: expand_ctyp.ml,v 0.0 2012/03/15 01:55:22 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf;
open Ppo;
open Util;
open Camlp4.PreCast;
(*
value __expand__ctyp__ = 
  [ <:ctyp@_loc< $id:id$ >> ->
  ]
;      
*)

value filter = object
  inherit Ast.map as super;
  value mutable in_expand=False;
  method! str_item x =
    match x with
    [
     <:str_item@_loc<
         value expand__ctyp__ = fun [ $match_cases$ ]
		  
      >>    ->	do {
    	p_match_case_r match_cases;
        let old = in_expand;
	in_expand := True;
    	let res = super#str_item x;
	in_expand := old ;
	res  
      }
    | _ -> begin
	(* p_str_item_r x ; *)
	(* prerr_endline "fuck"; *)
	super#str_item x ;
    end
    ];
  method! match_case x =
    match x with
    [ <:match_case< <:ctyp@_loc< $uid:fid$ >>  -> $_$  >> when in_expand -> do{

      (* prerr_endline fid; *)
      prerr_endline "good";
      super#match_case x;
      }
    | _ -> super#match_case x ]
    ;
      
end;
AstFilters.register_str_item_filter (filter#str_item);

















