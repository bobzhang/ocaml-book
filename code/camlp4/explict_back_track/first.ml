open Printf
open Camlp4.PreCast

let pr = Gram.Entry.print
let expr = Gram.Entry.mk "expr"
let test = Gram.Entry.of_parser "test" 
  (fun strm ->
    match Stream.npeek 2 strm with 
    |  [_ ; KEYWORD "xyzzy", _ ] -> raise Stream.Failure
    | _ -> ())
    
let _ = begin
  EXTEND Gram GLOBAL:expr ; 
  g : [[ "plugh" ]] ;
  f1 : [[ g ; "quux" ]];
  f2 : [[g ; "xyzzy"]] ; 
  expr :
    [[test ; f1 -> print_endline "1" | f2 -> print_endline "2" ]] ;
  END ;
  pr Format.std_formatter expr;
  Gram.parse_string expr (Loc.mk "<string>") "plugh xyzzy"
end 





















