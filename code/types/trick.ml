(* Version: $Id: trick.ml,v 0.0 2012/02/18 18:15:29 bobzhang1988 Exp $ *)
open Printf

type t == OtherLib.t =
    A
  | B
  | C
      
(** This will make the compiler verify that OtherLib.t = A | B | C |
    ... and make the two types equal. I don't think it really solves
    your case, as you probably don't want to change either
    library.'  *)


















