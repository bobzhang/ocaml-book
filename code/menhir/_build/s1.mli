exception Error

type token = 
  | TIMES of ( int->int->int )
  | RPAREN
  | PLUS of ( int->int->int )
  | MINUS of ( int->int->int )
  | LPAREN
  | INT of (int)
  | EOL
  | DIV of ( int->int->int )


val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (int)