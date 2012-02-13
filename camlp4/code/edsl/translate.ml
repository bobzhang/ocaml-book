(* Author: bobzhang1988@seas215.wlan.seas.upenn.edu        *)
(* Version: $Id: translate.ml,v 0.0 2012/02/12 20:28:50 bobzhang1988 Exp $ *)


(** Revised Syntax *)

(* open BatPervasives; *)
open Printf;
open Test; 
open Camlp4;
open Camlp4.PreCast;


value rec concat  (asts : list Ast.expr) : Ast.expr  =
    match asts with
   [ [] -> assert False
   | [h] -> h 
   | [h::t ] ->
     let _loc = Ast.loc_of_expr h in 
     <:expr< $h$ ^ $concat t$  >>
   ]
;
      
value rec translate (p: prog ) :Ast.str_item =
  match p with
      [ Prog(_loc,stmts) ->
	<:str_item< $list: List.map translate_stmt stmts $ >>
      ]
and translate_stmt (st:stmt) = 
    match st with
    [ Def (_loc, id, expr) ->
      <:str_item< value $lid:id$ = $translate_expr expr$ ;>>
    | Print (_loc, exprs) ->
      <:str_item< print_endline $concat  (List.map translate_expr exprs) $;
	>>
    ]
and translate_expr (e:expr) =
    match e with
    [ Var (_loc, id) -> <:expr<$lid:id$ >>
    | String (_loc,s) -> <:expr<$str:s$ >>    (* Check whether needs escaped later*)
    ]
;

begin
  Gram.Entry.clear Syntax.str_item ;
  let open Syntax in begin 
  EXTEND Gram GLOBAL:str_item;
    str_item : [ [ s = pys_eoi -> translate s ] ];
  END ;
  (* Gram.Entry.print Format.std_formatter expr; *)
  end ;
end;















(* module Pa_python (Syntax:Sig.Camlp4Syntax) = struct *)
(*   (\* open Syntax; *\) *)
(*   prerr_endline "why is it not invoked"; *)
(*   open Camlp4.PreCast.Syntax; *)
(*   Gram.Entry.clear str_item ; *)
(*   EXTEND Gram *)
(*     str_item : [ [ s = pys_eoi -> *)
(*       begin *)
(* 	prerr_endline "here"; *)
(* 	translate s; *)
(*       end ] ]; *)
(*   END ; *)
(*   include Syntax ; *)
(* end ; *)

(* prerr_endline "weird"; *)
(* let module M = Camlp4.Register.OCamlSyntaxExtension Id Pa_python in (); *)

(** Here we use MGram, they are _actually_ the same, but
    Syntax.Gram.Entry.t is not the same as
    Test.Prog Test.MGram.Entry.t 
*)

(* Without functors *)
(* This is the most straightforward. We complete our code from above as follows: *)
