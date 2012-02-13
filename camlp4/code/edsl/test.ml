(* Author: bobzhang1988@seas215.wlan.seas.upenn.edu        *)
(* Version: $Id: test.ml,v 0.0 2012/02/12 19:48:30 bobzhang1988 Exp $ *)
(* open BatPervasives *)

(* ocamlbuild -lflags -linkall translate.cma *)
(* camlp4o -I _build translate.cma -impl test.py -printer o *)
open Printf
open Camlp4.PreCast

open Camlp4

(**
   1. Define your own ast
      There's need to add location to your ast.
      This can locate the error position during type-check time
      If you only care syntax-error location, then it's not necessary 
   2. Parse to your ast
   3. Translate yor ast to ocaml ast
      two ways to translate
      a. meta-expr
         mechanical, but not very useful, just make your concrete syntax
         a little easy
      b. do some transformation 
         semantics changed 
   4. define quotation syntax for your syntax tree
      not necessary but make your life easier 
*)

(**
   concrete_syntax
   stmt:
      def id = expr
      print exprs
   expr:
      string-literal
      id

   exprs: expr list 
*)


(* module MGram = MakeGram(Lexer) *)
module MGram = Gram
  
type  stmt =
    Def of Loc.t *  id * expr
  | Print of Loc.t * expr list
and  expr =
  | Var of Loc.t * id
  | String of Loc.t * string
and id = string
and prog =
  | Prog of Loc.t * stmt list 

let pys = MGram.Entry.mk "pys"
let pys_eoi = MGram.Entry.mk "pys_eoi"

let _ = begin
  MGram.Entry.clear pys;
  MGram.Entry.clear pys_eoi;
  EXTEND MGram GLOBAL: pys_eoi;
  pys_eoi:
    [ [ prog= pys ; EOI -> prog ]];
  END;
  EXTEND MGram GLOBAL:pys;
    pexpr: [
      [ s = STRING -> String(_loc, s)
	(** here we want  "\n" be comprehended as "\n", so we don't
	    escape.
	    *)
      | id = LIDENT -> Var(_loc, id)
      ]
    ];
    pys:
      [ [ stmts = LIST0 [ p = py ;  ";" -> p  ]  -> Prog(_loc, stmts)] ];
    py:
      [
	[ "def"; id=LIDENT; "="; e = pexpr -> begin
	  (* prerr_endline "def"; *)
	  Def (_loc, id, e);
	end 
	| "print"; es = LIST1 pexpr SEP "," ->
	   Print(_loc, es)
	]
      ];
  END;
end

let pys_parser  str =
  MGram.parse_string pys_eoi (Loc.mk "<string>") str

let a =
  pys_parser "def a = \"3\"; def b =  \"4\"; def c =  \"5 \"; print a,b,c; "

(** Parser is ok now.  Now Ast Transformer, if we defined quotation
    for our own syntax. That would be easier *)




