(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: util.ml,v 0.0 2012/03/07 02:06:06 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf

let (|>) x f = f x

let failwithf fmt = ksprintf failwith fmt 

let  (^$) f x = f x

















