(** #require "camlp4.gramlib"*)
open Camlp4.PreCast
  
module MGram = MakeGram(Lexer)
  
let m_expr = MGram.Entry.mk "m_expr";;
let pr = MGram.Entry.print
let _ =
  EXTEND MGram
    GLOBAL: m_expr ;
    m_expr : 
      [[ "foo"; f  -> print_endline "first"
       | "foo" ; "bar"; "baz" -> print_endline "second"]
      ]; 
    f : [["bar"; "baz" ]];  END;;

let _ = pr Format.std_formatter m_expr 
let _ = MGram.parse_string m_expr (Loc.mk "<string>") "foo bar baz ";;

(** output
    m_expr: [ LEFTA
    [ "foo"; "bar"; "baz"
    | "foo"; f ] ]
    second
*)


(** DELETE_RULE expr: SELF; "+"; SELF END;; *)
let _ = begin
  MGram.Entry.clear m_expr;
  EXTEND MGram GLOBAL: m_expr ;
    m_expr : 
     [[ "foo"; f  -> print_endline "first"
      | "foo" ; "bar"; "bax" -> print_endline "second"]
     ]; 
    f : [["bar"; "baz" ]];
  END;
  pr Format.std_formatter m_expr ; 
  MGram.parse_string m_expr (Loc.mk "<string>") "foo bar baz "
end 
(** output:
   m_expr: [ LEFTA
    [ "foo"; "bar"; "bax"
    | "foo"; f ] ]
  first
*)

let _ = begin
  MGram.Entry.clear m_expr;
  EXTEND MGram GLOBAL: m_expr ;
    m_expr : 
     [[ "foo"; f  -> print_endline "first"
      | "foo" ; "bar"; f -> print_endline "second"]
     ]; 
    f : [["bar"; "baz" ]];
  END;
  pr Format.std_formatter m_expr;
  MGram.parse_string m_expr (Loc.mk "<string>") "foo bar baz "
end 
(**
   m_expr: [ LEFTA
   [ "foo"; "bar"; f
   | "foo"; f ] ]
   Exception:
   Loc.Exc_located (<abstr>,
   Stream.Error "[f] expected after \"bar\" (in [m_expr])").
*)  


