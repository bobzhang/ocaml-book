open Batteries
open Sexplib
open Sexplib.Std
type token =
    Def
  | Extern
  | Ident of string
  | Number of float 
  | Kwd of char 
  | Plus    (** needed, yascc need to specify its precedence*) 
  | Minus
  | Mul 
  | Div 
  | LParen
  | RParen 
  | Semi 
  | Comma
with sexp 

let string_of_token = 
  sexp_of_token |- Sexp.to_string
let print_token = string_of_token |- print_string 
