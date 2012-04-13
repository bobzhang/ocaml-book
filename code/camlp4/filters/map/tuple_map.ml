
open Camlp4.PreCast ;
value rec duplicate n t _loc =
    if n <= 0 then invalid_arg "duplicate n < 0"
    else
      let arr = Array.init n (fun x -> x) in 
      let lst = Array.(
	to_list (mapi (fun i _ -> <:patt< $lid: "x" ^ string_of_int i $ >> ) arr )) in 
      let patt = <:patt< $tup:Ast.paCom_of_list lst $ >> in 
      <:expr< fun $patt$ -> $tup:Ast.exCom_of_list
	  (Array.(
	   to_list (mapi (fun i _ ->
	     <:expr< $lid:t$ $lid:"x" ^ string_of_int i $ >> ) arr ))) $ >> ;
value map_filter = object
  inherit Ast.map as super ;
  method! expr e =
    match e with
    [ <:expr@_loc< Tuple.map $lid:f$ $tup:xs$ >> ->
      <:expr< $tup: Ast.(exCom_of_list  (List.map (fun e -> <:expr< $lid:f$ $e$ >> )
			   (list_of_expr xs []))) $ >>
     (** we can do the same as the second branch, but here we do inlining *)
    | <:expr@_loc< Tuple.map $lid:f$ $int:num$ >> ->
	let n = int_of_string num in
	super#expr
	  (duplicate n f _loc)
    | <:expr@_loc< Tuple.map $_$ >> ->
	failwith ("Tuple.map not transformed, supported two forms\n"^
		  "Tuple.map lid tup \n"^
		  "Tuple.map lid n \n")
    | others -> super#expr others ]
	;
end ;

AstFilters.register_str_item_filter map_filter#str_item;

(**

   camlp4rf -str 'value a = <:ctyp< ( $x$ * $list:xs$ ) >> ;' -printer o
   let a = Ast.TyTup (_loc, (Ast.TySta (_loc, x, (Ast.tySta_of_list xs))))   
 *)	
(**
   transform <:expr< ((1,3),2,3,4) >> <:expr< f >> |> p_expr;
   ((f (1, 3)), (f 2), (f 3), (f 4))

   (1,3) is different from 1,3, the latter is incomplete
   data constructor generic programming may help
   list_of_expr did the same thing actually

value rec comma_f f tups =
  match tups with
  [ <:expr@_loc< $l$,$r$ >> ->
    <:expr< $comma_f f l$, $comma_f f r$ >>
  | one  ->
      let _loc= Ast.loc_of_expr one in 
      <:expr< $lid:f$ $one$ >>
  ]
;  

value transform expr f  =
  match expr with
  [ <:expr@_loc< ($tup:tups$) >> ->
     <:expr< $tup:comma_f  f tups $ >>
  | _ -> invalid_arg "transform tup" ]
;
*)
