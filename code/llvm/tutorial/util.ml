(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: util.ml,v 0.0 2012/02/21 05:00:00 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
let (^$) f x = f x 
let (|>) x f = f x
let (|-) f g x = g(f(x))

let prerr_endlinef  fmt = ksprintf prerr_endline fmt


















