(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: first.ml,v 0.0 2012/03/05 15:53:38 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Camlp4.PreCast
module MGram = MakeGram(Lexer)
  
let test = MGram.Entry.mk "test"
let p = MGram.Entry.print 


let _ = begin
  MGram.Entry.clear test;
  EXTEND MGram GLOBAL: test;
  test:
    ["add" LEFTA
	[SELF; "+" ; SELF | SELF; "-" ; SELF]
    | "mult" RIGHTA
	[SELF; "*" ; SELF | SELF; "/" ; SELF]
    | "simple" NONA
	[ "("; SELF; ")"  | INT ] ];
  END;
  p Format.std_formatter test;
end 

(** output
 test: [ "add" LEFTA
        [ SELF; "+"; SELF
        | SELF; "-"; SELF ]
      | "mult" RIGHTA
        [ SELF; "*"; SELF
        | SELF; "/"; SELF ]
      | "simple" NONA
        [ "("; SELF; ")"
        | INT ((_)) ] ]

*)  


let _ = begin
  EXTEND MGram GLOBAL: test;
    test: [[ x = SELF ; "plus1plus" ; y = SELF ]];
  END ;
  p Format.std_formatter test
end
(** output
test: [ "add" LEFTA
    [ SELF; "plus1plus"; SELF
  | SELF; "+"; SELF
  | SELF; "-"; SELF ]
| "mult" RIGHTA
  [ SELF; "*"; SELF
  | SELF; "/"; SELF ]
| "simple" NONA
  [ "("; SELF; ")"
  | INT ((_)) ] ]
*)  


let _ = begin
  EXTEND MGram  test: LAST
    [[x = SELF ; "plus1plus" ; y = SELF ]];
  END;
  p Format.std_formatter test 
end 
(** output
              test: [ "add" LEFTA
  [ SELF; "plus1plus"; SELF
  | SELF; "+"; SELF
  | SELF; "-"; SELF ]
| "mult" RIGHTA
  [ SELF; "*"; SELF
  | SELF; "/"; SELF ]
| "simple" NONA
  [ "("; SELF; ")"
  | INT ((_)) ]
| LEFTA
  [ SELF; "plus1plus"; SELF ] ]
*)

  
let _ = begin
  EXTEND MGram  test: LEVEL "mult" [[x = SELF ; "plus1plus" ; y = SELF ]]; END ;
  p Format.std_formatter test;
end 
(** output
      test: [ "add" LEFTA
  [ SELF; "plus1plus"; SELF
  | SELF; "+"; SELF
  | SELF; "-"; SELF ]
| "mult" RIGHTA
  [ SELF; "plus1plus"; SELF
  | SELF; "*"; SELF
  | SELF; "/"; SELF ]
| "simple" NONA
  [ "("; SELF; ")"
  | INT ((_)) ]
| LEFTA
  [ SELF; "plus1plus"; SELF ] ]    
*)


let _ = begin
  EXTEND MGram  test: BEFORE "mult" [[x = SELF ; "plus1plus" ; y = SELF ]];
  END ;
  p Format.std_formatter test;
end
(** output
        test: [ "add" LEFTA
  [ SELF; "plus1plus"; SELF
  | SELF; "+"; SELF
  | SELF; "-"; SELF ]
| LEFTA
  [ SELF; "plus1plus"; SELF ]
| "mult" RIGHTA
  [ SELF; "plus1plus"; SELF
  | SELF; "*"; SELF
  | SELF; "/"; SELF ]
| "simple" NONA
  [ "("; SELF; ")"
  | INT ((_)) ]
| LEFTA
  [ SELF; "plus1plus"; SELF ] ]
*)
  
  



















