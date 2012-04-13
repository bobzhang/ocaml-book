open Camlp4.PreCast
val p_expr_r :
  Ast.expr -> unit
val p_expr_o : Ast.expr -> unit
val s_expr_r :
  Ast.expr -> string
val s_expr_o : Ast.expr -> string
val p_ident_r :
  Ast.ident -> unit
val p_ident_o : Ast.ident -> unit
val s_ident_r :
  Ast.ident -> string
val s_ident_o : Ast.ident -> string
val p_patt_r :
  Ast.patt -> unit
val p_patt_o : Ast.patt -> unit
val s_patt_r :
  Ast.patt -> string
val s_patt_o : Ast.patt -> string
val p_str_item_r :
  Ast.str_item -> unit
val p_str_item_o : Ast.str_item -> unit
val s_str_item_r :
  Ast.str_item -> string
val s_str_item_o : Ast.str_item -> string
val p_ctyp_r :
  Ast.ctyp -> unit
val p_ctyp_o : Ast.ctyp -> unit
val s_ctyp_r :
  Ast.ctyp -> string
val s_ctyp_o : Ast.ctyp -> string
val p_match_case_r :
  Ast.match_case -> unit
val p_match_case_o : Ast.match_case -> unit
val s_match_case_r :
  Ast.match_case -> string
val s_match_case_o : Ast.match_case -> string
val parse_expr : string -> Camlp4.PreCast.Syntax.Ast.expr
val parse_ident : string -> Camlp4.PreCast.Syntax.Ast.ident
val parse_patt : string -> Camlp4.PreCast.Syntax.Ast.patt
val parse_str_item : string -> Camlp4.PreCast.Syntax.Ast.str_item
val parse_ctyp : string -> Camlp4.PreCast.Syntax.Ast.ctyp
val parse_match_case : string -> Camlp4.PreCast.Syntax.Ast.match_case
