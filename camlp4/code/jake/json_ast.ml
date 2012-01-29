
(**
   camlp4of -filter map -filter fold -filter trash  -filter meta -parser Pa_type_conv.cma pa_sexp_conv.cma pa_json_ast.ml -printer o
*)
open Sexplib.Std
  
type float' = float
and t = 
  | Jq_null 
  |Jq_bool of bool 
  |Jq_number of float' 
  |Jq_string of string 
  |Jq_array of t list 
  |Jq_object of (string*t) list
with sexp
