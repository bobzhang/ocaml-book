(***********************************************************************)
(*                                                                     *)
(*                            pa_scope                                 *)
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

let rec drop n = function
  | h::t as l -> if n > 0 then drop (n-1) t else l
  | [] -> []

let stream_peek_from_nth n m strm = 
  drop n (List.map fst (Stream.npeek (n + m) strm))

let test_scope_delim name delims =
  Gram.Entry.of_parser ("test_scope_delim_" ^ name)
    (let len = List.length delims in
     let rec test lev dlev dot flag strm = 
       let seg = stream_peek_from_nth lev len strm in
       if dlev > 0 then
         match seg with
         | KEYWORD "(" :: _ -> test (lev+1) (dlev + 1) dot flag strm
         | KEYWORD ")" :: _ -> test (lev+1) (dlev - 1) dot flag strm
         | h :: _ -> test (lev+1) dlev dot flag strm
         | [] -> raise Stream.Failure
       else match seg with
       | KEYWORD s :: _  when flag && 
           (List.for_all 
              (fun (a,b) -> List.exists ((=) a) b)
              (List.combine seg delims)) ->  s.[0] <- '@'
       | KEYWORD "." :: _ -> test (lev+1) dlev false flag strm
       | KEYWORD "(" :: _ -> 
           let flag = flag 
             || stream_peek_from_nth (lev+1) 1 strm = [KEYWORD"struct"] in
           test (lev+1) (dlev+1) dot flag strm
       | UIDENT _ :: _ ->
           if dot then raise Stream.Failure
           else test (lev+1) dlev true true strm
       | _ -> raise Stream.Failure in
     fun strm -> test 0 0 false false strm)

let test_scope_delim_open = test_scope_delim "open" [[KEYWORD "@"]]
let test_scope_delim_close = 
  test_scope_delim "close" [[KEYWORD "."]; [KEYWORD "("; KEYWORD "{"]]

let fresh_name = let r = ref 0 in fun () -> incr r; "ModScope" ^ string_of_int !r

let open_e _loc = function
  | <:module_expr< $uid:i$ >> -> <:str_item< open $uid:i$ >>
  | m -> let modu = fresh_name() in
    <:str_item< module $modu$ = $m$ open $uid:modu$ >>

let open_n _loc m e =
  let modu = fresh_name() in
  <:expr< 
    let module $modu$ = struct 
      $open_e _loc m$ 
      let result = $e$ 
    end in $uid:modu$.result >>

let open_m _loc m e = match e with
  | <:expr< let module $n$ = $is$ in $uid:mn$.$me$ >> when n = mn -> 
    begin match is with
      (* match is with <:module_expr< struct $me$ >> doesn't work! *)
    | Ast.MeStr(_loc, ls) ->
        let nis = <:str_item< $open_e _loc m$ ;; $ls$ >> in
        <:expr< let module $n$ = struct $nis$ end in $uid:mn$.$me$ >>
    | _ -> open_n _loc m e end
  | _ -> open_n _loc m e

EXTEND Gram
GLOBAL: expr;
expr: BEFORE "top"
  [ "@" RIGHTA
      [ test_scope_delim_open; m = module_expr; "@"; e = expr LEVEL ";" ->
          open_m _loc m e ] ]
;
expr: LEVEL "."
  [ [ test_scope_delim_close; m = module_expr; "@"; e = expr -> 
        open_m _loc m e
    ] ]
;
END
