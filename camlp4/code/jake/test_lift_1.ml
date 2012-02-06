let loc = Loc.ghost
in
  Ast.StTyp (loc,
    (Ast.TyDcl (loc, "t", [],
       (Ast.TySum (loc,
          (Ast.TyOr (loc, (Ast.TyId (loc, (Ast.IdUid (loc, "A")))),
             (Ast.TyId (loc, (Ast.IdUid (loc, "B")))))))),
       [])))


