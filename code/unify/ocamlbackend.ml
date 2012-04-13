(* Author: bobzhang1988@vpl406.wlan.library.upenn.edu        *)
(* Version: $Id: backend.ml,v 0.0 2012/02/17 02:11:25 bobzhang1988 Exp $ *)
(** Revised Syntax *)
open Printf;
open Util;
open Mlast;

open Camlp4.PreCast;
(**FIXME location error
   Write your own meta filter??
*)
value _loc = Loc.ghost ;

value rec transform  (e:Mlast.ml_exp) =
  match e with
    [ <:me< $int:i$ >> -> <:expr< $`int:i $ >>
    | <:me< $bool:b$ >> -> <:expr< $`bool:b$ >>
    | <:me< fst $e$ >> -> 
	<:expr< Pervasives.fst $transform e$ >>
    | <:me< snd $e$ >> ->
      <:expr< Pervasives.snd $transform e$ >>
    | <:me< $e1$ + $e2$ >> ->
      <:expr< $transform e1 $ + $transform e2$ >>
    | <:me< $e1$ - $e2$ >> ->
      <:expr< $transform e1 $ + $transform e2$ >>
    | <:me< $e1$ * $e2$ >> ->
      <:expr< $transform e1 $ + $transform e2$ >>
    | <:me< $e1$ = $e2$ >> ->
      <:expr< $transform e1 $ = $transform e2$ >>
    | <:me< $e1$ < $e2$ >> ->
            <:expr< $transform e1 $ < $transform e2$ >>
    | <:me< $e1$ > $e2$ >> -> 
            <:expr< $transform e1 $ > $transform e2$ >>
    | <:me< ($e1$, $e2$ ) >> ->
      <:expr< ( $transform e1$ , $transform e2$ ) >>
    | <:me< $lid:x$ >> ->
      <:expr< $lid:x$ >>
    | <:me< if $e1$ then $e2$ else $e3$ >> ->
      <:expr< if $transform e1$ then $transform e2$ else $transform e3$ >>
    | <:me< fun $patt:x$ -> $e$ >> ->
      <:expr< fun $lid:x$ -> $transform e$ >>
    | <:me< $e1$ $e2$ >> ->
      <:expr< $transform e1$ $transform e2$ >>
    | <:me< let $patt:x$ = $e1$ in $e2$ >> ->
      <:expr< let $lid:x$ = $transform e1$ in $transform e2$ >>
    | <:me< let rec $patt:x$ = $e1$ in $e2$ >> -> 
      <:expr< let rec $lid:x$ = $transform e1$ in $transform e2$ >>
					      
    | Ml_Ant (_,_)
    | Ml_fun (Ml_patAnt (_,_),_ )
    | Ml_let (Ml_patAnt (_,_),_ ,_ )
    | Ml_letrec (Ml_patAnt (_,_),_ ,_ )-> failwithf "not supported ast "
    ]
;

(* value  generate_code_filter = Ast.map_str_item begin fun x -> *)
(*   match x with *)
(*     [ <:str_item< $exp: <:me< _ >>  $ >>  $ >> *)
(*       -> <:str_item< $x$ ; $ <:str_item< $exp:transform x' $ >> $ >>  *)
(*     | e -> e ] *)
(* end ; *)
(* AstFilters.register_str_item_filter generate_code_filter#str_item; *)

















