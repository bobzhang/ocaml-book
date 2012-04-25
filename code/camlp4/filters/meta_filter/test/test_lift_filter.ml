(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: test_lift_filter.ml,v 0.0 2012/02/19 05:20:10 bobzhang1988 Exp $ *)

(* #directory "_build/" *)


(* #load "lift_filter.cma" *)


open Printf


open Camlp4.PreCast


let _loc = Loc.ghost



type t =
    A | B


module Camlp4Trash = struct 
    INCLUDE "test_type.ml"
end 
  
(* open Lift_filter *)


open Util





module MetaExpr = Camlp4Filters.MetaGeneratorExpr(Test_type)


module MetaPatt = Camlp4Filters.MetaGeneratorPatt(Test_type)










