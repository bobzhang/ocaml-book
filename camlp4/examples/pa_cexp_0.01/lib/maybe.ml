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

type 'a t = Succ of 'a | Fail of exn

let zero = Failure "Fail"

let delay f = f ()

let return x = Succ x

let bind (x, f) = match x with
  | Succ v ->  f v
  | Fail _ as r -> r

let let_bind (x, f) = f x

let try_with (x, f) = match x with
  | Fail e -> f e
  | Succ _  as r -> r;;

let combine = function
  | Fail e as r , _ -> r
  | Succ _, x2 -> x2



