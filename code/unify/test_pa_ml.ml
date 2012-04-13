(* Author: bobzhang1988@vpl729.wireless-pennnet.upenn.edu        *)
(* Version: $Id: test_pa_ml.ml,v 0.0 2012/02/16 15:20:57 bobzhang1988 Exp $ *)

#directory "_build";;
#load "pa_ml.cma";;
#load "ml_infer.cma";;

open Unify
open Type_synthesis
open Printf
open Mlast

let a =
  <:me< let rec fib  x = 
	  if x = 1
	  then 1
	  else fib (x - 1) + fib (x-2)
	in fib 
	>>

let b =
  <:me<
    let f = $a$
    in f
    >>

let x = 3
let c = <:me< $int:x$ >> 

let d x = match x with
  | <:me< $int:_$ >>
      -> true
  | _ -> false
  




let test_antiquot x =
  <:me< fun $patt:x$ -> $lid:x$  >>

let test_antiquot2 x =
  <:me< let $patt:x$ = 3 in $lid:x$ >> 

let x = a |> generate_type_constraints |> rob 			 





