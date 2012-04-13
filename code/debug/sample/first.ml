(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: first.ml,v 0.0 2012/02/25 20:22:26 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)

open Printf

let rec fib = function
  | 0 | 1 -> 1
  | n ->  fib (n-1) + fib (n-2)

let _ = begin
  for i = 1 to 10 do
    print_int (fib i);
    print_newline ();
  done 
end 
