open Camlp4.PreCast  
module Jq_ast = struct 
  type float' = float 
  type t = 
      Jq_null 
    |Jq_bool of bool 
    |Jq_number of float' 
    |Jq_string of string 

    |Jq_array of t  
    |Jq_object of t 
    |Jq_colon of t * t  (* to make an object *)
    |Jq_comma of t * t  (* to make an array *)
    |Jq_Ant of Loc.t * string 
    |Jq_nil (* similiar to StNil *)
  let rec t_of_list lst = match lst with 
    |[] -> Jq_nil 
    | b::bs -> Jq_comma (b, t_of_list bs)
end 
let (|>) x f = f x 

module MetaExpr = struct 
  let meta_float' _loc f = <:expr< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorExpr(Jq_ast)
end 
module MetaPatt = struct 
  let meta_float' _loc f = <:patt< $`flo:f$ >>
  include Camlp4Filters.MetaGeneratorPatt(Jq_ast)  
end 



module MGram = MakeGram(Lexer) 
let json  = MGram.Entry.mk "json"
let json_eoi = MGram.Entry.mk "json_eoi" 

let _ = let open Jq_ast in begin 
  EXTEND  MGram  GLOBAL: json_eoi  json ; 
  json_eoi : [[ x = json ; EOI -> x ]]; 
  json : 
    [[ "null" -> Jq_null 
     |"true" -> Jq_bool true 
     |"false" -> Jq_bool false 
     (** register special tags for anti-quotation*)  
     | `ANTIQUOT (""|"bool"|"int"|"floo"|"str"|"list"|"alist" as n , s) -> 
       Jq_Ant(_loc, n ^ ": " ^ s )
     | n = [ x = INT-> x | x =  FLOAT -> x ] -> Jq_number (float_of_string n)
     | "["; es = SELF ; "]" -> Jq_array es 
     | "{";  kvs = SELF ;"}" -> Jq_object kvs 
     | k= SELF; ":" ; v = SELF -> Jq_colon (k, v)
     | a = SELF; "," ; b = SELF -> Jq_comma (a, b)
     | -> Jq_nil  (* camlp4 parser epsilon has a lower priority *)
     ]];
  END ;
end 


let destruct_aq s  =
  let try  /(_* Lazy as  name ) ":" (_* as content)/ = s 
  in  name,content
  with Match_failure _ -> invalid_arg (s  ^ "in destruct_aq")

let aq_expander = object 
  inherit Ast.map as super 
  method expr = function 
    |Ast.ExAnt(_loc, s) -> 
      let n, c = destruct_aq s in
      (** use host syntax to parse the string  *)
      let e = Syntax.AntiquotSyntax.parse_expr _loc c in  begin
	match n with 
        |"bool" -> <:expr< Jq_ast.Jq_bool $e$ >> (* interesting *)
        |"int" -> <:expr< Jq_ast.Jq_number (float $e$ ) >>
        |"flo" -> <:expr< Jq_ast.Jq_number $e$ >>
        |"str" -> <:expr< Jq_ast.Jq_string $e$ >>
        | "list" -> <:expr< Jq_ast.t_of_list $e$ >>
        |"alist" -> 
          <:expr<
            Jq_ast.t_of_list 
            (List.map (fun (k,v) -> Jq_ast.Jq_colon (Jq_ast.Jq_string k, v))
            $e$ )
          >>
        |_ -> e 
      end 
    |e -> super#expr e 
  method patt = function 
    | Ast.PaAnt(_loc,s) -> 
      let n,c = destruct_aq s in 
      Syntax.AntiquotSyntax.parse_patt _loc c  (* ignore the tag *)
    | p -> super#patt p 
end 


let parse_quot_string _loc s = 
  let q = !Camlp4_config.antiquotations in 
  (** checked by the lexer to allow antiquotation 
      the flag is initially set to false, so antiquotations 
      appearing outside a quotation won't be parsed 
      *)
  Camlp4_config.antiquotations := true ; 
  let res =  MGram.parse_string  json_eoi _loc s in 
  Camlp4_config.antiquotations := q ; 
  res 

let expand_expr _loc _ s =   s 
  |> parse_quot_string _loc 
  |> MetaExpr.meta_t _loc
  (** aq_expander inserted here *)    
  |> aq_expander#expr 

let expand_str_item _loc _ s = 
  (**insert an expression as str_item *)
   <:str_item@_loc< $exp: expand_expr _loc None s $ >>

let expand_patt _loc _ s  = s 
  |> parse_quot_string _loc 
  |> MetaPatt.meta_t _loc
  (** aq_expander inserted here *)    
  |> aq_expander#patt 
let _  = let open Syntax.Quotation in  begin
  add "json" DynAst.expr_tag expand_expr ;
  add "json" DynAst.patt_tag expand_patt ;
  add "json" DynAst.str_item_tag expand_str_item ;
  default := "json";
end 
    
