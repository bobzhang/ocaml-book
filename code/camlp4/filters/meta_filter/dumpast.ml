(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: dumpast.ml,v 0.0 2012/03/08 15:08:40 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf;
open Util;
open Camlp4.PreCast;
open Common;

(** add a location to each constructor
    does not support type synonym right now
    only support variant type
    type t = [A | B of int and  (boo * string) ]
 *)
value type_module = "Location";

value with_loc  = object
  inherit Ast.map as super;
  value mutable name_of_loc  = "";
  value mutable name_to_loc  = "";
  value bindings = Hashtbl.create 50 ;
  value mutable names = [];
  method! ctyp x =
    match x with
    [ Ast.TyDcl _loc n tyvars branches constraints-> begin
      name_of_loc := ("dump_" ^ n ^ "_of_loc");
      name_to_loc := ("dump_" ^ n ^ "_to_loc");
      names :=  [ name_of_loc :: [name_to_loc :: names ] ];
      prerr_endlinef "Type name is %s" n;
      super#ctyp x ;
      (* match  super#ctyp x with  *)
      (* [ Ast.TyDcl _loc _ tyvars branches constraints -> *)
      (* 	Ast.TyDcl _loc (n^"_l") tyvars branches constraints *)
      (* | _ -> assert False *)
      (* ]; *)
    end
    | <:ctyp@_loc< $_$ | $_$ >> ->
      let lst = (Ast.list_of_ctyp x []) in
      let open List in 
      lst |> map (fun
	[ <:ctyp@_loc< $uid:con$ of $branch$ >> -> do  {

          let arity = (List.length (Ast.list_of_ctyp branch [] )) ;
          Hashtbl.add bindings name_of_loc 
	      <:match_case< $gen_patt arity "x" <:patt< $id: <:ident< $uid:type_module$ . $uid:con $ >> $ _loc >> $ ->
		($gen_expr arity "x" <:expr< $uid:con$ >> $, _loc) >>; 
          Hashtbl.add bindings name_to_loc 
             <:match_case< $gen_patt arity "x" <:patt< $uid:con$  >> $ ->
	     $gen_expr arity "x" <:expr< $id: <:ident< $uid:type_module$ . $uid:con$ >> $ _loc >> $  >> ; (** type was generated in a module *)
	   super#ctyp <:ctyp< $uid:con$ of Loc.t and $branch$ >> ;
          }
        | <:ctyp@_loc< $uid:con$ >>  -> do {
          let arity = 0 ;
          Hashtbl.add bindings name_of_loc 
	      <:match_case< $gen_patt arity "x" <:patt< $id: <:ident< $uid:type_module$ . $uid:con $ >> $_loc >> $ ->
		($gen_expr arity "x" <:expr< $uid:con$ >> $, _loc) >>; 
          Hashtbl.add bindings name_to_loc 
             <:match_case< $gen_patt arity "x" <:patt< $uid:con$  >> $ ->
	     $gen_expr arity "x" <:expr< $id: <:ident< $uid:type_module$ . $uid:con$ >> $ _loc >> $ >> ;

	  super#ctyp <:ctyp< $uid:con$ of Loc.t >>
          }
	| _ -> begin
	    prerr_endlinef "tyOr_of_list generate some unusual types ";
	    assert False;
	end 
        ] ) |> Ast.tyOr_of_list
   | other -> super#ctyp other ];
  method reset = begin 
    names := [];
    name_of_loc:= "";
    name_to_loc := "";
    Hashtbl.clear bindings;
  end;
  method functions =begin 
    let tr name =
        try 
	  let match_cases = Hashtbl.find_all  bindings name in
	  if ends_with name "to_loc" then 
	    <:binding< $lid:name$ _loc =
	    fun [ $ <:match_case< $list: match_cases$ >> $ ]
		>>
	  else
	    <:binding< $lid:name$  =
	    fun [ $ <:match_case< $list: match_cases$ >> $ ]
		>>
	    
	with
	  [
	   Not_found -> begin
	     prerr_endlinef "Iterating hashtbl canot found any match cases!";
	     assert False;
	   end 
	   ]
       in 
    match names with
    [ [] -> <:str_item< >>
    | [h::t] ->
	let binding =
	  List.fold_right (fun name acc -> <:binding< $tr name$ and $acc$ >> )
	  t (tr h) in
	<:str_item<
	value rec $binding$ >>
   ]
  end ;
end ;


value generate_loc ctyp = do{
  prerr_endline "generate_loc";
  with_loc#reset;
  let ty_def = with_loc#ctyp ctyp ;
  let dumps = with_loc#functions;
  <:str_item<
    module $type_module$ = struct 
      type $typ:ty_def$;
    end ;
    $dumps$
  >>;
}
;  


Type_gen.register [("dump",  generate_loc )];

(**
value emit  = object
  inherit Ast.map as super;
  method! str_item  str = 
    match str with
    [ <:str_item< type $typ:ty$ >> -> begin
      with_loc#reset;
      let ty_def = with_loc#ctyp ty in
      let dumps = with_loc#functions in
	<:str_item<
        $str$;
        type $typ:ty_def$ ;
        $dumps$
        >> ;
    end 
    | other -> super#str_item other
    ];
end ;
AstFilters.register_str_item_filter (emit#str_item); *)




(**
   type t  = u 
 *)

(**
value test_type = <:str_item<
type ml_unop = [ Ml_fst    | Ml_snd   ]
and ml_binop =
  [ Ml_add   | Ml_sub     | Ml_mult  
  | Ml_eq    | Ml_less    | Ml_gt   ]
and ml_exp =
  [ Ml_int_const of    int
  | Ml_bool_const of   bool
  | Ml_pair of   ml_exp and ml_exp
  | Ml_unop of   ml_unop and ml_exp 
  | Ml_binop of  ml_binop and ml_exp and ml_exp
  | Ml_var of   string
  | Ml_if of   ml_exp and ml_exp and ml_exp
  | Ml_fun of  ml_patt and ml_exp
  | Ml_app of  ml_exp and ml_exp
  | Ml_let of  ml_patt and ml_exp and ml_exp
  | Ml_letrec of ml_patt and ml_exp and ml_exp
  | Ml_Ant of   string  ](* and meta filter special treatment *)
and ml_type =
  [ Int_type    
  | Bool_type   
  | Pair_type of ml_type and ml_type 
  | Arrow_type of ml_type and ml_type   
  | Var_type of  string   
  | Type_Ant of  string    ]
and ml_patt =
  [ Ml_pat_id of  string   
  | Ml_patAnt of  string    ]
>>;



begin
  let <:str_item< type $typ:ty$ >> = test_type in 
  emit ty |> p_str_item;
end ;
*)











