open Camlp4.PreCast
open Json_ast

module Camlp4TrashX = struct
    INCLUDE "json_ast.ml"
end 

open Camlp4TrashX
    
class map = Camlp4MapGenerator.generated

class fold = Camlp4FoldGenerator.generated


  
module MetaExpr = struct 
  let meta_float' _loc f =
    <:expr< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorExpr(Camlp4TrashX)
end
  
module MetaPatt  = struct 
  let meta_float' _loc f =
    <:patt< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorPatt(Camlp4TrashX)  
end


  
