
open Aparser;
open Camlp4_config ;
open Ameta;
open Camlp4.PreCast ;

value parse_quot_string _loc s = do{
  let q = antiquotations.val; 
  antiquotations.val := True ;
  let res = t_of_string _loc s ;
  antiquotations.val := q;
  res   
};

value expand_expr _loc loc_name_opt s = do{
  let ast = parse_quot_string  _loc s ;
  MetaExpr.meta_t _loc ast;  
};

  
value expand_patt _loc loc_name_opt s = do{
  let res = parse_quot_string  _loc  s; 
  let patt_ast  = MetaPatt.meta_t _loc res;
  let rec subst_first_loc name = fun
    [ <:patt@_loc< Ast. $uid:u$ $_$ >>  ->
      <:patt< Ast. $uid:u$ $lid:name$ >>
    | <:patt@_loc< $a$ $b$ >> ->
	<:patt< $subst_first_loc name a$ $b$ >>
    | p -> p];
  match loc_name_opt with
  [ None -> patt_ast
  | Some name -> subst_first_loc  name patt_ast ]  
};

value expand_str_item _loc loc_name_opt s = do{
  let res = parse_quot_string _loc s;
  let ast = MetaExpr.meta_t _loc res;
  <:str_item< $exp:  ast $ >>    ;
};

let open Quotation in do{
  add "json" DynAst.expr_tag expand_expr;
  add "json" DynAst.patt_tag expand_patt;
  add "json" DynAst.str_item_tag expand_str_item;
};



