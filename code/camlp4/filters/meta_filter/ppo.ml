(**
   Generate 6 kinds of functions.

   [val p_expr_r : Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax).Ast.expr -> unit]

   [val p_expr_o : OPrinters.Ast.expr -> unit]

   [val s_expr_r : Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax).Ast.expr -> string]

   [val s_expr_o : OPrinters.Ast.expr -> string]

   [val parse_expr : string -> Camlp4.PreCast.Syntax.Ast.expr]

   the parser is made use of module {!Camlp4.PreCast.Syntax}
*)
DEFINE PRINTERS =[
expr;
ident;
patt;
str_item;
ctyp;
match_case;
]


DEFINE PARSERS = [
expr;
ident;
patt;
str_item;
ctyp;
match_case;
]


let _ = __gen__pp__(PRINTERS)

let _ = __gen__parser__ (PARSERS)



















