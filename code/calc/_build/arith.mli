type token =
  | INTEGER of (int)
  | PLUS
  | TIMES

val expr :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> int
