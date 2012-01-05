
type token =
  | NEWLINE
  | LPAREN
  | RPAREN
  | NUM of (float)
  | PLUS
  | MINUS
  | MULTIPLY
  | DIVIDE
  | CARET

val input :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
val exp :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> float

