TYPE_CONV_PATH "Test_sexp"

open Sexplib
open Sexplib.Sexp
open Sexplib.Conv
let (|>) x f = f x
(**

   if you don't provide, it will infer sexp.ml.t
*)
type t = A | B
with sexp


(** debug
    camlp4o -parser Pa_type_conv.cma pa_sexp_conv.cma   sexp.ml -printer o
*)    

type reco = {
  foo : int;
  bar : string;
} with sexp

(** To make use of sexp_option, Conv is required to open *)
open Conv
type reco2 = {
  x : int option;
  y : int sexp_option;
} with sexp

let a  = {x=Some 3 ; y = None} |> sexp_of_reco2;;

(** sexp_list sexp_array sexp_bool
    assumes empty as default value
*)    

type sum2 = A | B of int * float * sum2
with sexp

(** polymorphic variants here  *)

type ('a,'b) pv = [`X of ('a,'b) pv | `Y of 'a * 'b ]
with sexp

    
type 'a pt = A | B of 'a
with sexp

type foo = int pt
with sexp

(** ADT
    here write your own
*)    
    
(**
   Hashtbl
*)

(** opaque type *)


type foo2 = A of int * char sexp_opaque
with sexp

module M = struct
  exception Foo of int  with sexp
end

let b = M.Foo 3 |> sexp_of_exn

exception Foo2 of int
with sexp

let c = Foo2 4 |> sexp_of_exn
    
open Batteries
let _ =
  prerr_endline "uh";
  prerr_endline (dump b);
  prerr_endline (dump c);

  (** FIXME
     I don't know why this fails
     let strc = c |> string_of_sexp in
  *)
  let strc = c |> to_string_hum in
  prerr_endline strc;
  prerr_endline "xx";
  let str = c |> to_string_hum in 
  (** let str = b |> string_of_sexp in *)
  prerr_endline str 


let _ =
  prerr_endline "uh";
  prerr_endline (dump b);
  prerr_endline (dump c);
  let strc = c |> string_of_sexp in
  prerr_endline strc;
  prerr_endline "xx";
  let str = b |> string_of_sexp in
  prerr_endline str    
    
(**
   load_sexp_conv load_sexp_conv_exn
*)
