(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: second.ml,v 0.0 2012/03/05 05:58:02 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Camlp4.PreCast
let expr = Gram.Entry.mk "expr"

let pr = Gram.Entry.print
let fmt = Format.std_formatter 
let (|>) x f = f x 
let p = Gram.parse_string expr (Loc.mk "<string>")
  let t = "foo bar baz" 
let _ = begin
  EXTEND Gram GLOBAL: expr ;
    expr : 
      [[ "foo"; f  -> print_endline "first"
       | "foo" ; "bar"; "baz" -> print_endline "second"]
      ]; 
    f : [["bar"; "baz" ]];  END;
  END;
  p t |> printf "%s\n";
  let open Gram.Entry in begin 
  clear expr ;
  EXTEND Gram GLOBAL: expr;
    expr:
      [["foo";f -> print_endline "first"
      | "foo"; "bar"; "bax" -> print_endline "second"]];
    f: [["bar";"baz"]];
  END ;
  p t |> printf "%s\n";
  clear expr;
  EXTEND Gram GLOBAL: expr :
      expr:
      [["foo";f -> print_endline "first"
      |"foo";"bar"; f -> print_endline "second"]];
      f: [["bar";"baz"]];
  END;
  p t |> printf "%s\n";  
  end 
end 













