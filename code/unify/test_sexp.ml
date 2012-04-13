(* Author: bobzhang1988@vpl406.wlan.library.upenn.edu        *)
(* Version: $Id: test_sexp.ml,v 0.0 2012/02/17 01:27:06 bobzhang1988 Exp $ *)
open Printf

type ('a,'b) term = 
  | Term of 'a * ('a,'b) term list 
  | Var of 'b



open Camlp4.PreCast

type ('a,'b) delta =
    ('b *  ('a,'b) term) list
with sexp 

















