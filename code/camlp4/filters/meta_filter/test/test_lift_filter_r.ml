(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: test_lift_filter.ml,v 0.0 2012/02/19 05:20:10 bobzhang1988 Exp $ *)

(* #directory "_build/" *)
(* ; *)

(* #load "lift_filter.cma" *)
(* ; *)

open Printf;

open Camlp4.PreCast;

value _loc = Loc.ghost;


type t = [A of Loc.t | B of Loc.t ];    

module Camlp4Trash = struct 
    INCLUDE "test_type_r.ml";
end ;
  


open Util;


(* value a = ty_dcl1 |> gen_expr_match_case ;*)

module MetaExpr = Camlp4Filters.MetaGeneratorExpr(Test_type_r);

module MetaPatt = Camlp4Filters.MetaGeneratorPatt(Test_type_r);  









