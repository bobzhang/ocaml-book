open Camlp4.PreCast
(** open Batteries  *)
(** So the first one adds [%] to the simple level; it’s added as two
    terminals because the lexer splits [%] into two tokens [% and ];
    this isn’t a problem, we’ll just recognize them in sequence, but
    it does allow [% (* foo *) ] to stand for lazy nil.*)

let _ = let open Syntax in begin
  EXTEND Gram GLOBAL: patt;
  patt: LEVEL "simple"
    [
      [ "[%" ; "]" ->  <:patt< lazy Nil >> ] ];
  patt: LEVEL "::"
    [ [ p1 = SELF; "%::"; p2 = SELF ->
    <:patt< lazy Cons ($p1$,  $p2$ ) >>
      ] ];
  END;
end 

(**
open Camlp4.PreCast
  
(** open Batteries  *)
(** So the first one adds [%] to the simple level; it’s added as two
    terminals because the lexer splits [%] into two tokens [% and ];
    this isn’t a problem, we’ll just recognize them in sequence, but
    it does allow [% (* foo *) ] to stand for lazy nil.*)
let _ = let open Syntax
  in
    let _ = (patt : 'patt Gram.Entry.t)
    in
      (Gram.extend (patt : 'patt Gram.Entry.t)
         ((fun () ->
             ((Some (Camlp4.Sig.Grammar.Level "simple")),
              [ (None, None,
                 [ ([ Gram.Skeyword "[%"; Gram.Skeyword "]" ],
                    (Gram.Action.mk
                       (fun _ _ (_loc : Gram.Loc.t) ->
                          (Ast.PaLaz (_loc,
                             (Ast.PaId (_loc, (Ast.IdUid (_loc, "Nil"))))) :
                            'patt)))) ]) ]))
            ());
       Gram.extend (patt : 'patt Gram.Entry.t)
         ((fun () ->
             ((Some (Camlp4.Sig.Grammar.Level "::")),
              [ (None, None,
                 [ ([ Gram.Sself; Gram.Skeyword "%::"; Gram.Sself ],
                    (Gram.Action.mk
                       (fun (p2 : 'patt) _ (p1 : 'patt) (_loc : Gram.Loc.t)
                          ->
                          (Ast.PaLaz (_loc,
                             (Ast.PaApp (_loc,
                                (Ast.PaApp (_loc,
                                   (Ast.PaId (_loc,
                                      (Ast.IdUid (_loc, "Cons")))),
                                   p1)),
                                p2))) :
                            'patt)))) ]) ]))
            ()))
  


*)			
