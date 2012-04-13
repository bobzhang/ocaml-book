(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: ast_parser.ml,v 0.0 2012/02/21 02:17:57 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf


open Camlp4.PreCast
open Tast
open Util

  
let expr = Gram.Entry.mk "expr"
and proto : proto Gram.Entry.t = Gram.Entry.mk "proto"
and func : func Gram.Entry.t = Gram.Entry.mk "func"
and prog = Gram.Entry.mk "prog"
and progs = Gram.Entry.mk "progs"
and progs_eoi = Gram.Entry.mk "progs_eoi"
  
let mk_parser p =
  let p = Gram.parse_string p (Loc.mk "<string>") in
  fun s ->
    try  p s
    with
	Loc.Exc_located (t,exn) -> begin
	  prerr_endlinef "%s" (Loc.to_string t );
	  raise exn
	end 
	  
      
let expr_p = mk_parser expr 
and proto_p = mk_parser proto 
and func_p = mk_parser func  
and prog_p = mk_parser  prog 
and progs_p = mk_parser  progs
and progs_eoi_p = mk_parser progs_eoi
  
let _ = begin
  let clear = Gram.Entry.clear in
   clear expr;
   clear proto;
   clear func;
   clear progs;
   clear prog;
   clear progs_eoi ;
  EXTEND Gram GLOBAL:expr proto func progs prog progs_eoi;
  expr:
    [ 
     "<"
    [ x = SELF; "<"; y = SELF -> Binary('<',x,y)
    | x = SELF; ">"; y = SELF -> Binary('>',x,y)]
	
    | "+"
    [ x=SELF; "+"; y = SELF -> Binary ('+', x, y)
    | x=SELF; "-"; y = SELF -> Binary ('-', x, y)]

    | "*"
    [ x=SELF; "*"; y = SELF -> Binary ('*', x, y)
    | x=SELF; "/"; y = SELF -> Binary ('/', x, y)]	
    | "fun"
    [ f=LIDENT; "("; args = LIST0 SELF SEP "," ; ")"
      -> Call(f, Array.of_list args)]
    |"simple"
    [ `FLOAT(f,_ ) -> Number f
    | `INT (i,_) -> Number (float i)
    | x = LIDENT -> Variable x
    | "("; x=SELF; ")" -> x  ] ]
    ;
  proto:
    [ 
      ["extern"; f = LIDENT; "("; args = LIST0 [x=LIDENT->x] SEP ","; ")"
      -> Prototype(f, Array.of_list args)]]
    ;
  func:
    [["def"; f = LIDENT; "("; args=LIST0 [x=LIDENT->x] SEP "," ; ")"; body = expr ->
       Function(Prototype(f,Array.of_list args), body)
     ]]
    ;
  prog:
    [ [ p = proto -> Proto p
      | e = expr -> Expr e
      | d = func -> Func d ]
    ]
   ;
   progs:
     [
       [ ps = LIST0  [x=prog; ";"->x]  -> ps ]
     ]
     ;
   progs_eoi:
     [[x=progs; EOI -> x ]]
     ;
  END; 
end 



let print_expr  =  string_of_expr  |- print_string 
let print_progs = string_of_progs |- print_string  

(* let _ = begin *)
(*   "sin(a)" *)
(*   |> expr_p *)
(*   |> print_expr; *)
(*   "sin" *)
(*   |> expr_p |> print_expr; *)
(* end *) 


