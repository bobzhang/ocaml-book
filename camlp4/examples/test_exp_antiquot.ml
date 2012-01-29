

let anti _loc s  =
  <:str_item@_loc< $exp:s $>>

(** let anti _loc s = Ast.StSem (_loc, (Ast.StExp (_loc, s)),
    (Ast.StNil _loc)) *)
