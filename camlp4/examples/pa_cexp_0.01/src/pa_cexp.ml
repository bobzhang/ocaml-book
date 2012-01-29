(***********************************************************************)
(*                                                                     *)
(*                            pa_cexp                                  *)
(*                                                                     *)
(*             (C) 2008 by Zheng Li (li@pps.jussieu.fr)                *)
(*                                                                     *)
(*  This program is free software; you can redistribute it and/or      *)
(*  modify it under the terms of the GNU Lesser General Public         *)
(*  License version 2.1 as published by the Free Software Foundation,  *)
(*  with the special exception on linking described in file LICENSE.   *)
(*                                                                     *)
(*  This program is distributed in the hope that it will be useful,    *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of     *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      *)
(*  GNU Library General Public License for more details.               *)
(*                                                                     *)
(***********************************************************************)

open Camlp4.PreCast
open Syntax

let stream_peek_nth n strm =
  try Some (fst (List.nth (Stream.npeek n strm) (n-1)))
  with Failure "nth" | Invalid_argument "List.nth" -> None

let match_string s =
  Gram.Entry.of_parser ("match_string_"^s)
    (fun strm -> match Stream.peek strm with
     | Some ((LIDENT s' | UIDENT s'), _) when s' = s -> 
         Stream.junk strm; <:ident< $s'$ >>
     | _ -> raise Stream.Failure)

let use = match_string "use"
let return = match_string "return"
let finally = match_string "finally"

let test_not_record =
  Gram.Entry.of_parser "test_not_record" 
    (let rec pass_one lev dlev dot strm = match stream_peek_nth lev strm with
       | Some (KEYWORD ("(" | "{" | "[")) -> 
           pass_one (lev+1) (dlev+1) dot strm
       | Some (KEYWORD (")" | "}" | "]")) ->
           pass_one (lev+1) (dlev-1) (dlev=1) strm
       | Some (KEYWORD ("=" | "with")) when dlev = 0 -> raise Stream.Failure
       | Some (KEYWORD ".") when dlev = 0 ->
           pass_one (lev+1) dlev false strm
       | Some (KEYWORD _) when dlev = 0 -> ()
       | Some _ when dlev = 0 -> 
           if dot then () else pass_one (lev+1) dlev true strm
       | Some _ when dlev > 0 -> pass_one (lev+1) dlev dot strm
       | _ -> () in
     fun strm -> pass_one 1 0 false strm)


EXTEND Gram
GLOBAL: expr;
  expr: LEVEL "simple"
    [ [ "{"; test_not_record; e = cexp; "}" -> 
          <:expr< delay (fun () -> $e$) >>
      ] ]
  ;
  cexp: 
    [ 
      "par" RIGHTA
        [ c1 = SELF; ";" ; c2 = SELF -> 
            <:expr< combine ($c1$, delay(fun () -> $c2$)) >>
        | c1 = SELF; ";" -> c1 ]
    | "seq"
        [ "let"; "!"; p = patt; "="; e = expr; "in"; c = cexp LEVEL "par" ->
            <:expr< bind ($e$, (fun $p$ -> $c$)) >>
        | "let"; p = patt; "="; e = expr; "in"; c = cexp LEVEL "par" ->
            <:expr< let_bind ($e$, (fun $p$ -> $c$)) >>
        | _ = use; "!"; p = patt; "="; e = expr; "in"; c = SELF ->
            <:expr< bind($e$, (fun $p$ -> using ($e$, (fun $p$ -> $c$)))) >>
        | _ = use; p = patt; "="; e = expr; "in"; c = SELF ->
            <:expr< using($e$, (fun $p$ -> $c$)) >>
        | "do"; "!"; e = expr; "in"; c = SELF ->
            <:expr< bind($e$, (fun () -> $c$)) >>
        | "do"; e = expr; "in"; c = SELF ->
            <:expr< let_bind($e$, (fun () -> $c$)) >>
        | "for"; p = patt; "in"; e = expr; "do"; c = SELF; "done" ->
            <:expr< for_loop($e$, (fun $p$ -> $c$)) >>
        | "while"; e = expr; "do"; c = SELF; "done" ->
            <:expr< while_loop(fun () -> $e$, delay (fun () -> $c$)) >>
        | "if"; b = expr; "then"; c1 = SELF; "else"; c2 = SELF ->
            <:expr< if $b$ then $c1$ else $c2$ >>
        | "if"; b = expr; "then"; c1 = SELF ->
            <:expr< if $b$ then $c1$ else zero >>
        | _ = return; "!"; v = expr LEVEL "top" -> <:expr< $v$ >>
        | _ = return; v = expr LEVEL "top" -> <:expr< return $v$ >>
        | "try"; e = expr; "with"; a = match_case ->
            <:expr< try_with ($e$, function $a$) >>
        | "try"; e = expr; _ = finally; f = expr ->
            <:expr< try_finally ($e$, $f$) >>  ]
    ]
  ;
  END;

