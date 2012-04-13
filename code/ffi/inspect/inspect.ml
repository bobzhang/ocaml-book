(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: inspect.ml,v 0.0 2012/02/22 22:37:31 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Util

external inspect : 'a -> 'a = "inspect"

let rec fib  = function
  | 0 | 1 -> 1
  | n -> fib (n-1) + fib (n-2)

(* let i = inspect |- ignore      *)

let f  =  fun x y z -> x + y + z
let f2 = fun x y z -> x + y * z
type foo = {x:int;y:int;z:int}
type foo1 =
    C1 of int * int * int 
  | C2 of int
  | C3
  | C4 of int * int
let s = "aghosh\000aghoshgo"
let w : int Weak.t= Weak.create 10 
let _ = begin
  inspect 3 |> ignore;
  inspect 3.0 |> ignore ;
  inspect 'a' |> ignore;
  [|1;1;0|] |> inspect |> ignore;
  (1,true,()) |> inspect |> ignore ;
  {x=1;y=1;z=0} |> inspect |> ignore;
  C1(1,1,0) |> inspect |> ignore;
  [|1.0;1.0;0.0|] |> inspect |> ignore;
  inspect [1.0;1.0;0.0] |> ignore;

  fib |> inspect |> ignore ;  

  f |> inspect |> ignore;
  f 2 |> inspect |> ignore ;
  f 1 2 |> inspect |> ignore ;
  f2 1 2 |> inspect |> ignore ;
  let g = let x = 1 and y = 2 in fun z -> x + y + z in g |> inspect |> ignore;
  inspect |> inspect |> ignore;
  s |> inspect |> ignore;
  w |> inspect |> ignore;
  stdout |> inspect ;
end 


















