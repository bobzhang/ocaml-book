(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: plus.ml,v 0.0 2012/02/22 04:38:27 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
external unsafe_plus : int -> int -> int -> int -> int -> int -> int =
  "plus_six_bc"  "plus_six_nc"


let _ = begin
  print_int (unsafe_plus 1 2 3 4 5 6);
end 

















