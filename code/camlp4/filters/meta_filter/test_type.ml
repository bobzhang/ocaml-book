(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: test_type.ml,v 0.0 2012/02/19 21:51:23 bobzhang1988 Exp $ *)
open Printf
open Camlp4.PreCast

 
type ml_unop =
    Ml_fst | Ml_snd
and ml_binop =
   Ml_add | Ml_sub | Ml_mult | Ml_eq | Ml_less | Ml_gt
and ml_exp =
   Ml_int_const of  int
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
  | Ml_Ant of Loc.t * string  (* * meta filter special treatment *)
and ml_type =
   Int_type
  | Bool_type
  | Pair_type of ml_type * ml_type
  | Arrow_type of ml_type * ml_type
  | Var_type of string
  | Type_Ant of Loc.t * string 
and ml_patt =
   Ml_pat_id of string
  | Ml_patAnt of Loc.t * string 


















