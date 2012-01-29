(***********************************************************************)
(*                                                                     *)
(*                            pa_trybind                               *)
(*                                                                     *)
(*             (C) 2008 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)

open Camlp4.PreCast
open Syntax
open Ast

let pattalias _loc pl = 
  let r = ref 0 in
  List.fold_right (
    fun p l -> 
      (match p with
       | PaNil loc -> p, ExNil loc
       | PaId (loc, i) -> p, ExId (loc, i)
       | _ -> 
           incr r; let id = "__id_" ^ string_of_int !r in
           <:patt< $p$ as $id$ >>, <:expr< $lid:id$ >> )
      :: l
  ) pl []


EXTEND Gram
  GLOBAL: expr;
  expr: LEVEL "top"
  [ [ "let"; r = opt_rec; bi = binding;
      optmc = OPT ["with"; m = match_case -> m];
      "in"; ex = expr LEVEL ";" ->
        match optmc with
        | None -> <:expr< let $rec:r$ $bi$ in $ex$ >>
        | Some m ->
            let opl, oel = List.split (Ast.pel_of_binding bi) in
            let npl, nel = List.split (pattalias _loc opl) in
            let nbi = Ast.binding_of_pel (List.combine npl oel) in
            let net = match nel with 
              | [] -> assert false | [h] -> <:expr< $h$ >>
              | h::t -> <:expr< ($h$, $Ast.exCom_of_list t$) >> in
            let opt = match opl with
              | [] -> assert false | [h] -> <:patt< $h$ >>
              | h::t -> <:patt< ($h$, $Ast.paCom_of_list t$) >> in
            <:expr<
              match (try let $rec:r$ $nbi$ in `Succ $net$ with e -> `Fail e)
              with`Succ v -> let $opt$=v in $ex$ |`Fail e -> try raise e with $m$>>
    ] ]
  ;
END
