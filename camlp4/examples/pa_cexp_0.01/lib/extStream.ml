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

include Stream

(* Helper *)

let rec (--) a b = if a > b then [<>] else [<'a; a+1 -- b>]

let rec map (s, f) = 
  [<match s with parser [<'h>] -> [<'f h; map (s, f)>] | [<>] -> [<>]>]

let rec flatten ss =
  [<match ss with parser [<'h>] -> [<h; flatten ss>] | [<>] -> [<>]>]

let dup s =
  let rec gen s qa qb = 
    [< if not (Queue.is_empty qa) then [<'(Queue.take qa); gen s qa qb>]
       else match s with parser
       | [<'h>] -> Queue.add h qb;[<'h; gen s qa qb>] | [<>] -> [<>]
    >] in
  let q1 = Queue.create () and q2 = Queue.create () in
  (gen s q1 q2, gen s q2 q1)


(* Definition *)

let zero = [<>]

let return x = [<'x>]

let bind (s, f) = flatten(map(s, f))

let let_bind(x, f) = f x

let delay f = [<f ()>]

let combine (s1, s2) = [<s1; s2>]

let for_loop (s, f) = bind (s, f)

let rec while_loop (f, s) = 
  if f() then 
    let s,s' = dup s in
    [<s; while_loop(f, s')>]
  else [<>]

let try_with(s,f) = match s with parser
  | [<'h>] -> [<'h; s>] | [<>] -> f s

let try_finally(s,f) = match s with parser
  | [<'h>] -> [<'h; s>] | [<>] -> f(); s

