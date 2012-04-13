(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: tast.ml,v 0.0 2012/02/21 02:15:21 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Sexplib
open Std
open Util

type expr =
  (* variant for numeric literals like "1.0". *)
  | Number of float

  (* variant for referencing a variable, like "a". *)
  | Variable of string

  (* variant for a binary operator. *)
  | Binary of char * expr * expr

  (* variant for function calls. *)
  | Call of string * expr array
with sexp 

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto = Prototype of string * string array
with sexp

    
(* func - This type represents a function definition itself. *)
type func = Function of proto * expr
with sexp 

type prog =
  | Func of func
  | Proto of proto
  | Expr of expr 
with sexp
    
type progs = prog list       
with sexp
    

let string_of_expr  = sexp_of_expr |- Sexp.to_string_hum
let string_of_proto  =sexp_of_proto |- Sexp.to_string_hum 
let string_of_func  = sexp_of_func |- Sexp.to_string_hum
let string_of_prog = sexp_of_prog |- Sexp.to_string_hum 
let string_of_progs = sexp_of_progs |- Sexp.to_string_hum











