

open Camlp4.PreCast ;


(**
   transform <:expr< ((1,3),2,3,4) >> <:expr< f >> |> p_expr;
   ((f (1, 3)), (f 2), (f 3), (f 4))

   (1,3) is different from 1,3, the latter is incomplete
   data constructor generic programming may help 
 *)
value rec comma_f f tups =
  match tups with
  [ <:expr@_loc< $l$,$r$ >> ->
    <:expr< $comma_f f l$, $comma_f f r$ >>
  | one  ->
      let _loc= Ast.loc_of_expr one in 
      <:expr< $f$ $one$ >>
  ]
;  

value transform expr f  =
  match expr with
  [ <:expr@_loc< ($tup:tups$) >> ->
     <:expr< $tup:comma_f  f tups $ >>
  | _ -> invalid_arg "transform tup" ]
;
    
value map_filter = object
  inherit Ast.map as super ;
  method! expr e =
    match e with
    [ <:expr@_loc< Tuple.map $f$ $xs$ >> ->
      super#expr (transform xs f )
    | others -> super#expr others ]
	;
end ;

AstFilters.register_str_item_filter map_filter#str_item;

(**

   camlp4rf -str 'value a = <:ctyp< ( $x$ * $list:xs$ ) >> ;' -printer o
   let a = Ast.TyTup (_loc, (Ast.TySta (_loc, x, (Ast.tySta_of_list xs))))   
 *)	
