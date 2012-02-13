(* Author: bobzhang1988@seas215.wlan.seas.upenn.edu        *)
(* Version: $Id: expr.ml,v 0.0 2012/02/12 04:51:28 bobzhang1988 Exp $ *)
open BatPervasives
open Printf
open Camlp4.PreCast
(* Work out if an expression is an integer constant.
 *
 * Returns [Some i] if so (where i is the integer value), else [None].
 *
 * Fairly simplistic algorithm: we can only detect simple constant
 * expressions such as [k], [k+c], [k-c] etc.
 *)
let rec expr_is_constant = function
  | <:expr< $int:i$ >> ->	       (* Literal integer constant. *)
    Some (int_of_string i)
  | <:expr< $lid:op$ $a$ $b$ >> ->
    (match expr_is_constant a, expr_is_constant b with
     | Some a, Some b ->	       (* Integer binary operations. *)
         let ops = ["+", (+); "-", (-); "*", ( * ); "/", (/);
		    (* NB: explicit fun .. -> is necessary here to work
		     * around a camlp4 bug in OCaml 3.10.0.
		     *)
                    "land", (fun a b -> a land b);
		    "lor", (fun a b -> a lor b);
		    "lxor", (fun a b -> a lxor b);
                    "lsl", (fun a b -> a lsl b);
		    "lsr", (fun a b -> a lsr b);
		    "asr", (fun a b -> a asr b);
		    "mod", (fun a b -> a mod b)] in
         (try Some ((List.assoc op ops) a b) with Not_found -> None)
     | _ -> None)
  | _ -> None





let optimize_expr = object (self)
  inherit Ast.map as super
  method expr e = match expr_is_constant e with
    | Some i -> <:expr< $`int:i$ >>
    | None -> super#expr e 
end 




let b = optimize_expr#expr <:expr< let a = 32 lsr ( 1+2) in a  >>;;

(*
val b : Camlp4.PreCast.Ast.expr =    
Camlp4.PreCast.Ast.ExLet (<abstr>, Camlp4.PreCast.Ast.ReNil,
 Camlp4.PreCast.Ast.BiEq (<abstr>,
  Camlp4.PreCast.Ast.PaId (<abstr>, Camlp4.PreCast.Ast.IdLid (<abstr>, "a")),
  Camlp4.PreCast.Ast.ExInt (<abstr>, "4")),
 Camlp4.PreCast.Ast.ExId (<abstr>, Camlp4.PreCast.Ast.IdLid (<abstr>, "a")))
*)






