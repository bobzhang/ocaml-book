type token =
    Def
  | Extern
  | Ident of string
  | Number of float
  | Kwd of char
  | Plus
  | Minus
  | Mul
  | Div
  | LParen
  | RParen
  | Semi
  | Comma
with sexp

val string_of_token : token -> string
val print_token : token -> unit

