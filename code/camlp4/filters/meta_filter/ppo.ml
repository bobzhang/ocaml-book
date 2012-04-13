(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: printer_parser_of_ocaml.ml,v 0.0 2012/03/09 22:18:17 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)


DEFINE PRINTERS =[
 expr;
 ident;
 patt;
 str_item;
 ctyp;
 match_case;
]

DEFINE PARSERS = [
  expr;
  ident;
  patt;
  str_item;
  ctyp;
  match_case;
]


let _ = __gen__pp__(PRINTERS)

let _ = __gen__parser__ (PARSERS)

(**
   refer ppo.i.mli as reference
 *)    


















