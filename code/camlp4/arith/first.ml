(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: first.ml,v 0.0 2012/03/05 05:24:08 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Camlp4.PreCast
let (|>) x f = f x 
let expr = Gram.Entry.mk "expr"  
let _ = begin
  EXTEND Gram GLOBAL: expr;
  expr: [
  "add" LEFTA
  [ x = SELF; "+"; y = SELF -> x + y
  | x = SELF; "-"; y = SELF -> x - y ]
  |"mult" LEFTA
  [ x = SELF; "*"; y = SELF -> x * y
  | x = SELF; "/"; y = SELF -> x - y]
  |"pow" RIGHTA
  [ x = SELF; "**"; y = SELF -> int_of_float (float x ** float y)  ]
  |"simple" NONA
  [ `INT(x,_) -> x
  | "("; x = SELF; ")" -> x]
  ];
  END;
  Gram.Entry.print Format.std_formatter expr ;
  Gram.parse expr (Loc.mk "<string>")
    (Stream.of_string "3+ ((4-2)+28*3**2)+(4/2)") |> printf "%d\n";
end 

















