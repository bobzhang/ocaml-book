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
