(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: template.ml,v 0.0 2012/02/19 01:23:57 bobzhang1988 Exp $ *)


open Camlp4.PreCast
open Printf

module Mlast = struct 
  type int' = int 
  type ml_unop = Ml_fst | Ml_snd
  and ml_binop =
      Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less | Ml_gt
  and ml_exp =
    | Ml_int_const of  int'
    | Ml_bool_const of bool
    | Ml_pair of ml_exp * ml_exp
    | Ml_unop of ml_unop * ml_exp 
    | Ml_binop of ml_binop * ml_exp * ml_exp
    | Ml_var of string
    | Ml_if of ml_exp * ml_exp * ml_exp
    | Ml_fun of ml_patt * ml_exp
    | Ml_app of ml_exp * ml_exp
    | Ml_let of ml_patt * ml_exp * ml_exp
    | Ml_letrec of ml_patt * ml_exp * ml_exp
    | Ml_Ant of Loc.t * string  (** meta filter special treatment *)
  and ml_type =
    | Int_type
    | Bool_type
    | Pair_type of ml_type * ml_type
    | Arrow_type of ml_type * ml_type
    | Var_type of string
    | Type_Ant of Loc.t * string
  and ml_patt =
    | Ml_pat_id of string
    | Ml_patAnt of Loc.t * string 
end 
include Mlast
module MetaExpr :sig 
 val meta_ml_exp: Loc.t -> ml_exp -> Ast.expr
 val meta_ml_type: Loc.t -> ml_type -> Ast.expr
end = struct
  (**FIX camlp4 int bug*)
  let meta_int' _loc s = Ast.ExInt(_loc,string_of_int s)
  include Camlp4Filters.MetaGeneratorExpr(Mlast)
end
module MetaPatt : sig
  val meta_ml_exp : Loc.t -> ml_exp -> Ast.patt
  val meta_ml_type : Loc.t -> ml_type -> Ast.patt 
end = struct
  let meta_int' _loc s = Ast.PaInt(_loc, string_of_int s)
  include Camlp4Filters.MetaGeneratorPatt(Mlast)
end 














