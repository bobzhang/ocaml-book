open Camlp4.PreCast  
module MGram = MakeGram(Lexer) 
EXTEND MGram 
   GLOBAL: m_expr ;
    m_expr : 
     [[ "foo"; f  -> print_endline "first"
      | "foo" ; "bar"; "bax" -> print_endline "second"]
     ]; 
    f : [["bar"; "baz" ]];  END;;


(** translated code output *)
open Camlp4.PreCast
module MGram = MakeGram(Lexer)
let _ =
  let _ = (m_expr : 'm_expr MGram.Entry.t) in
  let grammar_entry_create = MGram.Entry.mk in
  let f : 'f MGram.Entry.t = grammar_entry_create "f"
  in
    (MGram.extend (m_expr : 'm_expr MGram.Entry.t)
       ((fun () ->
           (None,
            [ (None, None,
               [ ([ MGram.Skeyword "foo"; MGram.Skeyword "bar";
                    MGram.Skeyword "bax" ],
                  (MGram.Action.mk
                     (fun _ _ _ (_loc : MGram.Loc.t) ->
                        (print_endline "second" : 'm_expr))));
                 ([ MGram.Skeyword "foo";
                    MGram.Snterm (MGram.Entry.obj (f : 'f MGram.Entry.t)) ],
                  (MGram.Action.mk
                     (fun _ _ (_loc : MGram.Loc.t) ->
                        (print_endline "first" : 'm_expr)))) ]) ]))
          ());
     MGram.extend (f : 'f MGram.Entry.t)
       ((fun () ->
           (None,
            [ (None, None,
               [ ([ MGram.Skeyword "bar"; MGram.Skeyword "baz" ],
                  (MGram.Action.mk
                     (fun _ _ (_loc : MGram.Loc.t) -> (() : 'f)))) ]) ]))
          ()))
