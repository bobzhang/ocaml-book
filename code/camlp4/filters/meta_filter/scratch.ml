(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: scratch.ml,v 0.0 2012/03/09 18:07:17 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
module U = struct 
type a = A of int
end
open U
module V = struct     
type a = B of string | C of d
and  d = a
end 
open V 
let d = C (B "hello")



let map (f: 'a. 'a -> 'b)  (x::xs) = f x :: f xs     


module X = Y;


value f = fun [ <:str_item@_loc<
            module $name$ = $_$ >> -> 3] ;
















