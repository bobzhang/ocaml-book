open Camlp4.PreCast;
#default_quotation "expr";
value term = Gram.Entry.mk "term";
value term_eoi = Gram.Entry.mk "term_eoi";
EXTEND Gram GLOBAL: term term_eoi;
  term:
    [ "top"
      [ "\\"; v = var ; "->"; t = SELF -> 
      << `Lam  $v$ $t$ >> ]
    | "app"
	[ l = SELF ; r = SELF -> << `App $l$ $r$ >> ]
    | "simple"
      (** delegated to host language parser *)	
      [ `ANTIQUOT("", a) -> 
	Syntax.AntiquotSyntax.parse_expr _loc a
      | e = var  -> e
      | "("; e=SELF; ")" -> e ] ];
  var:
    (** here if we use $lid:l$ will result in unboud variable *)
    [ [l = LIDENT -> <:expr< `Var  $str:l$ >>  ] ];
  term_eoi: [ [ t = term; EOI  -> t ] ];
END;
(** wrap you parser for the interface *)
value expand_lambda_quot_expr _loc _loc_name_opt quotation_contents =
  let open Camlp4_config in do{
   let old = antiquotations.val;
   antiquotations.val := True;
   let res = Gram.parse_string term_eoi _loc quotation_contents ;
   antiquotations.val := old;
   res  
  }
;  

value expand_lambda_quot_str _loc _loc_name_opt quotation_contents =
  let open Camlp4_config in do {
    let old = antiquotations.val;
    antiquotations.val := True;
    let result = Gram.parse_string term_eoi _loc quotation_contents;
    antiquotations.val := old;
    <:str_item<$exp:result$ >> ; 
  }
;  

(**Register the expander now *)
let open Syntax.Quotation in begin
  add "lam" DynAst.expr_tag expand_lambda_quot_expr;
  add "lam" DynAst.str_item_tag expand_lambda_quot_str;
end ;

















