(* Author: bobzhang1988@vpl729.wireless-pennnet.upenn.edu        *)
(* Version: $Id: test_type_synthesis.ml,v 0.0 2012/02/16 21:53:15 bobzhang1988 Exp $ *)
open Printf

let b = let a =
  <:me< let rec fib  x = 
	  if x = 1
	  then 1
	  else fib (x - 1) + fib (x-2)
	in fib 
	>>
 in a |> generate_type_constraints |> rob ;;

















