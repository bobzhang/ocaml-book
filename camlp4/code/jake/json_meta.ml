open Camlp4.PreCast

open Json_ast
module J_ast = struct
  include Json_ast
end 

module MetaExpr : sig
  (* val meta_t : Loc.t -> Json_ast.t -> Ast.expr *)
end = struct 
  (** the generator scans all the types defined in the current module
      then generate code for the last-appearing recursive bundle
  *)
  let meta_float' _loc f =
    <:expr< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorExpr(J_ast)
  (* due to this can not run in toplevel *)
end
module MetaPatt : sig
  (* val meta_t : Loc.t -> Json_ast.t -> Ast.patt *)
end 
    = struct 
  let meta_float' _loc f =
    <:patt< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorPatt(J_ast)  
end


(* 
module MGram = MakeGram(Lexer)
let json_parser = MGram.Entry.mk "json"
let json_eoi = MGram.Entry.mk "json_eoi"  
let _ =   let open Jq_ast in begin
  EXTEND MGram  GLOBAL : json_parser ; 
  json_parser : 
     [["null" -> Jq_null 
     |"true" -> Jq_bool true
     |"false" -> Jq_bool false 
     | n = [x = INT -> x | y = FLOAT -> y  ] -> Jq_number (float_of_string n )
     | s = STRING -> Jq_string s 
     | "["; xs = LIST0 SELF SEP "," ; "]" -> Jq_array xs 
     | "{"; kvs = LIST0 [s = STRING; ":"; v = json_parser -> (s,v)] SEP ","; 
     "}" -> Jq_object kvs 
     ]] ; END ;
  EXTEND MGram  GLOBAL: json_eoi ; 
  json_eoi : [[x = json_parser ; EOI -> x ]] ;
  END ;
  MGram.parse_string json_eoi (Loc.mk "<string>") "[true,false]"
end 


let (|>) x f = f x 
let parse_quot_string _loc s = 
  MGram.parse_string  json_eoi _loc s 
let expand_expr _loc _ s =  s 
  |> parse_quot_string _loc 
  |> MetaExpr.meta_t _loc 
(** to make it able to appear in the toplevel *)
let expand_str_item _loc _ s = 
   (** exp antiquotation insert an expression as str_item *)
   <:str_item@_loc< $exp: expand_expr _loc None s $ >>
let expand_patt _loc _ s  =  s 
  |> parse_quot_string _loc 
  |> MetaPatt.meta_t _loc 
let _  = let open Syntax.Quotation in begin 
  add "json" DynAst.expr_tag expand_expr ;
  add "json" DynAst.patt_tag expand_patt ;
  add "json" DynAst.str_item_tag expand_str_item ;
  default := "json";
end
*)				   

(** make quotation from a parser *)
(**
  let install_quotation my_parser (me,mp) name = 
  let expand_expr _loc _ s = s |>  my_parser _loc |> me _loc in
  let expand_str_item _loc _ s =  <:str_item@_loc< $exp: expand_expr
    _loc None s $>> in
  let expand_patt _loc _ s = s |> my_parser _loc |> mp _loc in
  let open Syntax.Quotation in begin 
    add name DynAst.expr_tag expand_expr ;
    add "json" DynAst.patt_tag expand_patt ;
    add "json" DynAst.str_item_tag expand_str_item;
  end
*)
			  
      
