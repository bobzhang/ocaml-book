
open Camlp4.PreCast ;

module Camlp4Trash = struct
  INCLUDE "aast.ml";
end;

module MetaExpr = Camlp4Filters.MetaGeneratorExpr Aast ;
module MetaPatt = Camlp4Filters.MetaGeneratorPatt Aast ;  



