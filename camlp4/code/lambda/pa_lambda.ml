

(** Revised Syntax *)

open Camlp4.PreCast;


(**

module Caml = Camlp4OCamlParser.Make
  (Camlp4OCamlRevisedParser.Make Syntax);
value expr_of_string = Caml.Gram.parse_string Caml.expr_eoi
   
   The code above try to use built in caml syntax to save you time
   This is maily used in antiquotation syntax, since then it go back
   to host language actually doing this way is unwise, since you
   fixed your host language to be original syntax, actually you want
   it to be flexible

   The right way was to use
   Syntax.AntiquotSyntax.parse_expr (reflexitive)

   You also have Syntax.AntiquotSyntax.parse_patt (limited)
   for pattern expander
*)

module LambdaGram = MakeGram(Lexer)  ;

#default_quotation "expr";
value term = LambdaGram.Entry.mk "term";
value term_eoi = LambdaGram.Entry.mk "term_eoi";

EXTEND LambdaGram GLOBAL: term term_eoi;
  term:
    [ "top"
      [ "\\"; v = var ; "->"; t = SELF -> 
      << `Lam  $v$ $t$ >> ]
    |
      "app"
      [ l = SELF ; r = SELF -> << `App $l$ $r$ >> ]
    |
     "simple"
      (** delegated to host language parser *)	
      [ `ANTIQUOT("", a) -> begin
	prerr_endline a;
	Syntax.AntiquotSyntax.parse_expr _loc a
      end 
      | e = var  -> e
      | "("; e=SELF; ")" -> e ]
    	
    ];
  var:
    (** here if we use $lid:l$ will result in unboud variable *)
    [ [l = LIDENT -> <:expr< `Var  $str:l$ >>  ] ];
  term_eoi: [ [ t = term; EOI  -> t ] ];
END;

(** wrap you parser for the interface *)
value expand_lambda_quot_expr loc _loc_name_opt quotation_contents =
  let old = Camlp4_config.antiquotations.val in begin 
    Camlp4_config.antiquotations.val := True;
    (** need to turn it on, otherwise the lexer will not lex *)
    let res = LambdaGram.parse_string term_eoi loc quotation_contents in begin 
      Camlp4_config.antiquotations.val := old;
      res
    end 
  end 
;
(**Register the expander now *)
let open Syntax.Quotation in begin
  add "lam" DynAst.expr_tag expand_lambda_quot_expr;
end ;
