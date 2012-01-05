exception Error

type token = 
  | VAR of (string)
  | URI
  | TILDE
  | TERM_FUNC of (string)
  | STRING of (string)
  | SLASH
  | SEMICOLON
  | SEL_FUNC of (string)
  | S
  | QUOTIENT
  | QUANTITY of (Ast.quantity_t)
  | PLUS
  | PERIOD
  | PAGE
  | OPEN_SQUARE
  | OPEN_ROUND
  | OPEN_CURLY
  | NTH of (string)
  | MINUS
  | MEDIA
  | IMPORTANT
  | IMPORT
  | IDENT of (string)
  | HASH of (string)
  | GT
  | FONTFACE
  | EOF
  | DOUBLE_COLON
  | COMMA
  | COLON
  | CLOSE_SQUARE
  | CLOSE_ROUND
  | CLOSE_CURLY
  | CHARSET
  | ATTR_SUFFIX
  | ATTR_SUBSTRING
  | ATTR_PREFIX
  | ATTR_INCLUDES
  | ATTR_EQUALS
  | ATTR_DASHMATCH
  | ASTERISK


val stylesheet: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.t)