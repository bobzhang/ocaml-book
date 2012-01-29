(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006  Institut  National  de  Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

#default_quotation "expr";;

open Camlp4.PreCast
open Format

module FV = Camlp4.Struct.FreeVars.Make (Ast)

(** interesting *)
module PP = Camlp4.Printers.OCaml.Make (Syntax)
module S = FV.S

let _loc = Loc.ghost

let pervasives =
  let list =
    [ "+"; "-"; "/"; "*" (* ... *) ]
  in List.fold_right S.add list S.empty

(** FV.free_vars : S.t -> Ast.exr -> S.t *)
let f e =
  let fv = FV.free_vars pervasives e in
  S.fold (fun x acc -> <:expr< fun ~ $x$ -> $acc$ >> ) fv e

let print_expr = (new PP.printer ())#expr

let _ =
  printf "%a@." print_expr
    (f <:expr<let z = x + 2 in x + 2 * y * x * z>>)
