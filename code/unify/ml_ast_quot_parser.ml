(* Author: bobzhang1988@vpl342.wlan.library.upenn.edu        *)
(* Version: $Id: caml_ast_parser.ml,v 0.0 2012/02/16 05:43:46 bobzhang1988 Exp $ *)

open Printf
open Util
open Mlast
open Camlp4.PreCast
open Mlast_meta


(* open Ocamlbackend  *)
module MGram = MakeGram(Lexer)
let ml_exp, ml_exp_eoi = 
  MGram.Entry.mk "ml_exp", MGram.Entry.mk "ml_exp_eoi"
let ml_type, ml_type_eoi =
  MGram.Entry.mk "ml_type", MGram.Entry.mk "ml_type_eoi"

let clear = MGram.Entry.clear
let _ = begin
  clear ml_exp;  clear ml_exp_eoi;
  clear ml_type; clear ml_type_eoi;
  EXTEND MGram GLOBAL:ml_exp ml_exp_eoi ml_type ml_type_eoi ;
  ml_exp_eoi:
    [[ e = ml_exp; EOI -> e  ] ]
  ;
  ml_type_eoi:
    [[e = ml_type; EOI -> e]]
  ;
  ml_type:
    [ "arrow" RIGHTA
	[t1=SELF; "->"; t2 = SELF -> Arrow_type (t1,t2)]
    | "simple"
	[ "'"; i=LIDENT -> Var_type ("'" ^ i)
	| "("; t=SELF; ")" -> t
	| "("; l=SELF; "*"; r=SELF; ")" -> Pair_type (l,r)
	| "bool" -> Bool_type
	| "int" -> Int_type
	| `ANTIQUOT("",s) -> Type_Ant(_loc,s)
	]]
  ;
  ml_exp:
    [ "top"
      [ "let"; r = opt_rec ; bi=LIDENT; "=" ; e = SELF ; "in"; x = SELF ->
         if r then Ml_letrec (Ml_pat_id bi,e,x)
	 else Ml_let (Ml_pat_id bi,e,x)
      | "let"; r = opt_rec; bi=LIDENT;
	ids = LIST0 [x=LIDENT->x]; "="; e = SELF; "in"; x=SELF ->

        let m_fun = List.fold_right (fun id x ->
	  Ml_fun(Ml_pat_id id,x)) ids e in 
        if r then
	  Ml_letrec (Ml_pat_id bi,m_fun,x)
	else
	  Ml_let(Ml_pat_id bi,m_fun,x)
      | "let"; r = opt_rec; `ANTIQUOT(("patt" as n),s); "="; e = SELF;
	"in"; x=SELF ->
	if r then
	  Ml_letrec (Ml_patAnt(_loc,n^":"^s),e,x)
	else
	  Ml_let (Ml_patAnt(_loc,n^":"^s),e,x)
	    
      | "if"; b=SELF; "then"; l=SELF; "else"; r=SELF ->  Ml_if (b,l,r)
      | "fun"; l=LIDENT; "->"; e=SELF ->
         Ml_fun (Ml_pat_id l,e)
      | "fun"; `ANTIQUOT(("patt" as n),s) ; "->"; e = SELF ->
         Ml_fun(Ml_patAnt(_loc, n^":"^s),e) (** needs a new antiquot *)
      ]
    | "<" LEFTA
      [ el = SELF; "<"; er=SELF -> Ml_binop(Ml_less,el,er)
      | el = SELF; ">"; er=SELF -> Ml_binop(Ml_gt,el,er)
      | el = SELF; "="; er=SELF -> Ml_binop(Ml_eq,el,er) ]
    | "+" LEFTA
      [ el =SELF ;"+"; er=SELF -> Ml_binop (Ml_add,el,er)
      | el =SELF; "-"; er=SELF -> Ml_binop (Ml_sub,el,er)]
    | "*"
      [ el=SELF; "*"; er=SELF -> Ml_binop(Ml_mult,el,er) ]	
    | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> Ml_app (e1, e2)
      | "fst"; e = SELF -> Ml_unop(Ml_fst,e)
      | "snd"; e = SELF -> Ml_unop(Ml_snd,e)]
    | "simple"
      [ "("; e= SELF;")" -> e
      | "("; el=SELF; ","; er=SELF; ")"-> Ml_pair (el,er)
      | a = INT -> Ml_int_const (int_of_string a)
      | a = "true" -> Ml_bool_const true
      | a = "false" -> Ml_bool_const false
      | a = LIDENT -> Ml_var a
	(** this is just for parsing
	    the real meat lies in aq_expander
	*)
      | `ANTIQUOT( (""|"int"|"bool"|"lid") as n ,s) -> begin
	(* prerr_endline (sprintf "antiquot string %s" s); *)
	Ml_Ant(_loc,(n^":"^s));
      end ] ]
  ;
  opt_rec:
    [["rec" -> true
     |  -> false ]]
  ;
  END ;
end 
let parser_of_entry  entry str =
  try
    MGram.parse_string entry (Loc.mk "<string>") str
  with
      Loc.Exc_located(t,exn)-> begin
	prerr_endline (Loc.to_string t);
	raise exn;
      end 

let type_parser = parser_of_entry ml_type_eoi
let exp_parser = parser_of_entry ml_exp_eoi 


let test ()  =
  let test_exp_data =
    ["fun x -> x x  + 2 "] in
  let test_type_data =
    ["int-> int -> (bool * bool) "] in 
  (List.map exp_parser test_exp_data,
   List.map type_parser test_type_data)


let parse_expr_string _loc s =
  let q = !Camlp4_config.antiquotations in
  Camlp4_config.antiquotations := true ;
  let res = MGram.parse_string ml_exp_eoi _loc s in 
  Camlp4_config.antiquotations := q ;
  res

(* let destruct_aq s = *)
(*  Str.(bounded_split (regexp ":") s 2 ) *)
let destruct_aq s = String.(
  try
    let idx = index s ':' in
    (sub s 0 idx, sub s (idx + 1)(length s - idx - 1 ))
  with Not_found ->
    ("", s)
)


let prerr_endlinef s = ksprintf prerr_endline s   
let aq_expander = object 
  inherit Ast.map as super 
  method expr = function
    |Ast.ExAnt(_loc,s) -> begin 
      let n,c = destruct_aq s in
      prerr_endlinef  "n:%sc:%s" n c;
      let e = Syntax.AntiquotSyntax.parse_expr _loc c in begin 
        match n with
	  | "int" ->
	    <:expr< Ml_int_const $e$ >>
	  | "bool" ->
	    <:expr< Ml_bool_const $e$ >>
	  | "lid" ->
	    <:expr< Ml_var $e$ >>
	  | "patt" ->
	    <:expr< Ml_pat_id $e$ >> 
	  | "" -> e 
          | _ -> failwithf "unsupported expander %s in expr context" n 
      end;
    end
    | e -> super#expr e
  method patt = function
    |Ast.PaAnt(_loc,s) -> begin
      let n,c = destruct_aq s in
      let e = Syntax.AntiquotSyntax.parse_patt _loc c in begin
	prerr_endlinef "antiquot n : %s, c: %s" n c ;
	(match n with
	  | "int" ->
	    <:patt< Ml_int_const $e$ >>
	  | "bool" ->
	    <:patt< Ml_bool_const $e$ >>
	  | "lid" ->
	    <:patt< Ml_var $e$ >>
	  | "patt" ->
	    <:patt< Ml_pat_id $e$ >> 
	  | "" -> e
	  | _ ->
	    failwithf "unsupported expander %s in patt context" n 
	)
	;
      end 
    end
    | e -> super#patt e 
end 

let (|>) x f = f x 
let expand_expr _loc _ s = s
    |> parse_expr_string _loc
    |> MetaExpr.meta_ml_exp _loc
    |> aq_expander#expr

let expand_str_item _loc _ s : Ast.str_item =
  let e= expand_expr _loc None s in
  <:str_item@_loc< $exp: e $ >> (* ; $exp:transform e$ >> *)
    
let expand_patt _loc _ s = s
    |> parse_expr_string _loc
    |> MetaPatt.meta_ml_exp _loc
    |> aq_expander#patt 
    
let _ = let open Syntax.Quotation in begin
  add "me" DynAst.expr_tag expand_expr ;
  add "me" DynAst.str_item_tag expand_str_item ;
  add "me" DynAst.patt_tag expand_patt;
end

(**
   If you want to make it quotation expander
   you must parse it to camlp4 ast, if you want to support
   antiquotate syntax you must re-design your ast
*)    



















