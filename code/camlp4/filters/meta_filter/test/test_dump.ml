
(* open Camlp4.PreCast; *)
(* open Test_type_r; *)
(* module Camlp4Trash = struct *)
(*   INCLUDE "test_type_r.ml"; *)
(* end;  *)


(**
ocamlc -I +camlp4  -pp 'camlp4rf -I _build dumpast.cma'  test_dump.ml 

camlp4rf -I _build dumpast.cma test_dump.ml -printer r
*)  


open Camlp4.PreCast;
open Test_type_r;
module Location =
  struct
    type (* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: test_type.ml,v 0.0 2012/02/19 21:51:23 bobzhang1988 Exp $ *)
      ml_unop =
      [ Ml_fst of Loc.t | Ml_snd of Loc.t ]
      and ml_binop =
      [ Ml_add of Loc.t
      | Ml_sub of Loc.t
      | Ml_mult of Loc.t
      | Ml_eq of Loc.t
      | Ml_less of Loc.t
      | Ml_gt of Loc.t ]
      and ml_exp =
      [ Ml_int_const of Loc.t and int
      | Ml_bool_const of Loc.t and bool
      | Ml_pair of Loc.t and ml_exp and ml_exp
      | Ml_unop of Loc.t and ml_unop and ml_exp
      | Ml_binop of Loc.t and ml_binop and ml_exp and ml_exp
      | Ml_var of Loc.t and string
      | Ml_if of Loc.t and ml_exp and ml_exp and ml_exp
      | Ml_fun of Loc.t and ml_patt and ml_exp
      | Ml_app of Loc.t and ml_exp and ml_exp
      | Ml_let of Loc.t and ml_patt and ml_exp and ml_exp
      | Ml_letrec of Loc.t and ml_patt and ml_exp and ml_exp
      | Ml_Ant of Loc.t and string ]
      and (* and meta filter special treatment *)
      ml_type =
      [ Int_type of Loc.t
      | Bool_type of Loc.t
      | Pair_type of Loc.t and ml_type and ml_type
      | Arrow_type of Loc.t and ml_type and ml_type
      | Var_type of Loc.t and string
      | Type_Ant of Loc.t and string ]
      and ml_patt =
      [ Ml_pat_id of Loc.t and string | Ml_patAnt of Loc.t and string ];
  end;
value rec dump_ml_patt_to_loc _loc =
  fun
  [ Ml_patAnt x0 -> Location.Ml_patAnt _loc x0
  | Ml_pat_id x0 -> Location.Ml_pat_id _loc x0 ]
and dump_ml_type_of_loc =
  fun
  [ Location.Type_Ant _loc x0 -> ((Type_Ant x0), _loc)
  | Location.Var_type _loc x0 -> ((Var_type x0), _loc)
  | Location.Arrow_type _loc x0 x1 -> ((Arrow_type x0 x1), _loc)
  | Location.Pair_type _loc x0 x1 -> ((Pair_type x0 x1), _loc)
  | Location.Bool_type _loc -> (Bool_type, _loc)
  | Location.Int_type _loc -> (Int_type, _loc) ]
and dump_ml_type_to_loc _loc =
  fun
  [ Type_Ant x0 -> Location.Type_Ant _loc x0
  | Var_type x0 -> Location.Var_type _loc x0
  | Arrow_type x0 x1 -> Location.Arrow_type _loc x0 x1
  | Pair_type x0 x1 -> Location.Pair_type _loc x0 x1
  | Bool_type -> Location.Bool_type _loc
  | Int_type -> Location.Int_type _loc ]
and dump_ml_exp_of_loc =
  fun
  [ Location.Ml_Ant _loc x0 -> ((Ml_Ant x0), _loc)
  | Location.Ml_letrec _loc x0 x1 x2 -> ((Ml_letrec x0 x1 x2), _loc)
  | Location.Ml_let _loc x0 x1 x2 -> ((Ml_let x0 x1 x2), _loc)
  | Location.Ml_app _loc x0 x1 -> ((Ml_app x0 x1), _loc)
  | Location.Ml_fun _loc x0 x1 -> ((Ml_fun x0 x1), _loc)
  | Location.Ml_if _loc x0 x1 x2 -> ((Ml_if x0 x1 x2), _loc)
  | Location.Ml_var _loc x0 -> ((Ml_var x0), _loc)
  | Location.Ml_binop _loc x0 x1 x2 -> ((Ml_binop x0 x1 x2), _loc)
  | Location.Ml_unop _loc x0 x1 -> ((Ml_unop x0 x1), _loc)
  | Location.Ml_pair _loc x0 x1 -> ((Ml_pair x0 x1), _loc)
  | Location.Ml_bool_const _loc x0 -> ((Ml_bool_const x0), _loc)
  | Location.Ml_int_const _loc x0 -> ((Ml_int_const x0), _loc) ]
and dump_ml_exp_to_loc _loc =
  fun
  [ Ml_Ant x0 -> Location.Ml_Ant _loc x0
  | Ml_letrec x0 x1 x2 -> Location.Ml_letrec _loc x0 x1 x2
  | Ml_let x0 x1 x2 -> Location.Ml_let _loc x0 x1 x2
  | Ml_app x0 x1 -> Location.Ml_app _loc x0 x1
  | Ml_fun x0 x1 -> Location.Ml_fun _loc x0 x1
  | Ml_if x0 x1 x2 -> Location.Ml_if _loc x0 x1 x2
  | Ml_var x0 -> Location.Ml_var _loc x0
  | Ml_binop x0 x1 x2 -> Location.Ml_binop _loc x0 x1 x2
  | Ml_unop x0 x1 -> Location.Ml_unop _loc x0 x1
  | Ml_pair x0 x1 -> Location.Ml_pair _loc x0 x1
  | Ml_bool_const x0 -> Location.Ml_bool_const _loc x0
  | Ml_int_const x0 -> Location.Ml_int_const _loc x0 ]
and dump_ml_binop_of_loc =
  fun
  [ Location.Ml_gt _loc -> (Ml_gt, _loc)
  | Location.Ml_less _loc -> (Ml_less, _loc)
  | Location.Ml_eq _loc -> (Ml_eq, _loc)
  | Location.Ml_mult _loc -> (Ml_mult, _loc)
  | Location.Ml_sub _loc -> (Ml_sub, _loc)
  | Location.Ml_add _loc -> (Ml_add, _loc) ]
and dump_ml_binop_to_loc _loc =
  fun
  [ Ml_gt -> Location.Ml_gt _loc
  | Ml_less -> Location.Ml_less _loc
  | Ml_eq -> Location.Ml_eq _loc
  | Ml_mult -> Location.Ml_mult _loc
  | Ml_sub -> Location.Ml_sub _loc
  | Ml_add -> Location.Ml_add _loc ]
and dump_ml_unop_of_loc =
  fun
  [ Location.Ml_snd _loc -> (Ml_snd, _loc)
  | Location.Ml_fst _loc -> (Ml_fst, _loc) ]
and dump_ml_unop_to_loc _loc =
  fun [ Ml_snd -> Location.Ml_snd _loc | Ml_fst -> Location.Ml_fst _loc ]
and dump_ml_patt_of_loc =
  fun
  [ Location.Ml_patAnt _loc x0 -> ((Ml_patAnt x0), _loc)
  | Location.Ml_pat_id _loc x0 -> ((Ml_pat_id x0), _loc) ];
  
