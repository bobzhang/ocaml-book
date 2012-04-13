(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: str_quot.ml,v 0.0 2012/03/16 19:41:41 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Camlp4.PreCast
(* module Caml = *)
(*  Camlp4OCamlParser.Make *)
(*    (Camlp4OCamlRevisedParser.Make *)
(*      (Camlp4.OCamlInitSyntax.Make(Ast)(Gram)(Quotation)));; *)

let quotexpander str =
 "[1; 2; 3]" 

let patt_quotexpander loc _loc_name_opt str =
 Gram.parse_string Syntax.patt loc (quotexpander str)

let expr_quotexpander loc _loc_name_opt str =
 Gram.parse_string Syntax.expr loc (quotexpander str)

let str_item_quotexpander loc loc_name_opt str =
 <:str_item@loc< $exp: expr_quotexpander loc loc_name_opt str$ >>

let () =
 Syntax.Quotation.add "" Syntax.Quotation.DynAst.expr_tag expr_quotexpander;
 Syntax.Quotation.add "" Syntax.Quotation.DynAst.str_item_tag
str_item_quotexpander;
 Syntax.Quotation.add "" Syntax.Quotation.DynAst.patt_tag patt_quotexpander 

(**
   camlp4o -I _build str_quot.cmo -str "<<3 >> " -printer o

   [1;2;3]
 *)
















