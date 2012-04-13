val x : ?_loc:Camlp4.PreCast.Ast.loc -> int -> Camlp4.PreCast.Ast.ident
val meta_ :
  ?_loc:Camlp4.PreCast.Ast.loc -> string -> Camlp4.PreCast.Ast.ident
val mf_ : string -> string
val string_list_of_ident : Camlp4.PreCast.Ast.ident -> string list
val fill_ident_with_prefix :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  string -> Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.ident
val fold_left_i : ('a -> int -> 'a -> 'a) -> 'a -> 'a list -> 'a
val gen_patt_n :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  int -> string -> Camlp4.PreCast.Ast.patt -> Camlp4.PreCast.Ast.patt
val patt_of_data_ctor_n :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  int -> Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.patt
val gen_expr_n :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  int -> string -> Camlp4.PreCast.Ast.expr -> Camlp4.PreCast.Ast.expr
val expr_of_data_ctor_n :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  int -> Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.expr
val is_antiquot_data_ctor : string -> bool
val meta_ident : Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.expr
val me_of_id :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.expr
val mp_of_id :
  ?_loc:Camlp4.PreCast.Ast.loc ->
  Camlp4.PreCast.Ast.ident -> Camlp4.PreCast.Ast.expr
val failure : Camlp4.PreCast.Ast.expr
val failure_loc : Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr
val me_app :
  Camlp4.PreCast.Ast.expr ->
  Camlp4.PreCast.Ast.expr ->
  Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr
val mp_app :
  Camlp4.PreCast.Ast.expr ->
  Camlp4.PreCast.Ast.expr ->
  Camlp4.PreCast.Ast.loc -> Camlp4.PreCast.Ast.expr
val fold_data_ctors :
  Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax).Ast.ctyp ->
  (string -> Camlp4.PreCast.Ast.ctyp list -> 'a -> 'a) -> 'a -> 'a
val f_of_ctyp :
  Camlp4.PreCast.Ast.ctyp ->
  Camlp4.PreCast.Ast.expr * Camlp4.PreCast.Ast.expr ->
  Camlp4.PreCast.Ast.expr
