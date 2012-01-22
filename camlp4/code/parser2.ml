(**hand-coded entry  MGram.Entry.of_parser *)
open Camlp4.PreCast
module MGram = MakeGram(Lexer)
let pr = MGram.Entry.print
let test = MGram.Entry.of_parser "test" 
  (fun strm -> match Stream.npeek 2 strm with 
      [_ ; KEYWORD "xyzzy", _ ] -> raise Stream.Failure
    | _ -> ()) 
let m_expr = MGram.Entry.mk "m_expr"
  
let _ = begin
  EXTEND MGram GLOBAL: m_expr ; 
  g : [[ "plugh" ]] ;
  f1 : [[ g ; "quux" ]];
  f2 : [[g ; "xyzzy"]] ; 
  m_expr : [[test ; f1 -> print_endline "1" | f2 -> print_endline "2" ]] ;
  END ;
  pr Format.std_formatter m_expr;
  MGram.parse_string m_expr (Loc.mk "<string>") "plugh xyzzy"
end 

(**
   m_expr: [ LEFTA
   [ test; f1
   | f2 ] ]
   2
*)  
