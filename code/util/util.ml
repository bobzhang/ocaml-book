(* Version: $Id: util.ml,v 0.0 2012/02/17 20:19:56 bobzhang1988 Exp $ *)
open Printf
let (|>) x f = f x
let (^$) f x = f x
let failwithf fmt = ksprintf failwith fmt  
let prerr_endlinef fmt = ksprintf prerr_endline fmt
