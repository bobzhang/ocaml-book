(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: mlast_meta.ml,v 0.0 2012/02/20 01:26:45 bobzhang1988 Exp $ *)
open Printf
open Camlp4.PreCast
module Camlp4Trash = struct
    INCLUDE "mlast.ml"
    (** @include for code generation then tramp it *)  
end

(* class map = Camlp4MapGenerator.generated; *)

(* class fold = Camlp4FoldGenerator.generated; *)

module MetaExpr :sig
 val meta_ml_exp: Loc.t -> Mlast.ml_exp -> Ast.expr
 val meta_ml_type: Loc.t -> Mlast.ml_type -> Ast.expr
end = struct
  include Camlp4Filters.MetaGeneratorExpr(Mlast)
  (** @generate code by meta-filter *)  
end

  
module MetaPatt : sig
  val meta_ml_exp : Loc.t -> Mlast.ml_exp -> Ast.patt
  val meta_ml_type : Loc.t -> Mlast.ml_type -> Ast.patt
end = struct
  include Camlp4Filters.MetaGeneratorPatt(Mlast)
  (** @generate code by meta-filter *)  
end 


















