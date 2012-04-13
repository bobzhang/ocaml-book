(* Author: bobzhang1988@vpl729.wireless-pennnet.upenn.edu        *)
(* Version: $Id: util.ml,v 0.0 2012/02/16 21:30:48 bobzhang1988 Exp $ *)
open Printf

let (|>) x f = f x
let (^$) f x = f x

let failwithf fmt = ksprintf failwith fmt  

















