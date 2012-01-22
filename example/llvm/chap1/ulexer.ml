
open Batteries
open Ulex_util
open Sexplib
open Sexplib.Std
let main = Aparser.main 

type token =
  | DEF
  | EXTERN
  | PLUS
  | MINUS
  | MUL
  | DIV
  | LPAREN
  | RPAREN
  | SEMI
  | COMMA
  | EOF
  | NUMBER of float
  | KWD of char
  | IDENT of string
with sexp

(* open Aparser *)

let regexp newline = ('\010' | '\013' | "\013\010")
let regexp blank = [' ' '\009' '\012']
let regexp blanks = blank +
let regexp lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let regexp uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let regexp whitespace = (blank | newline)
let regexp underscore = "_"
let regexp tilde = "~"
let regexp identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let regexp symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let regexp lident = lowercase identchar *
let regexp uidnet = uppercase identchar * 
let regexp decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let regexp hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let regexp oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let regexp bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let regexp int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal
let regexp float_literal =
  ['0'-'9'] ['0'-'9' '_']*  ('.' ['0'-'9' '_']* )? (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']* )?

let first = ref true

let rec alex pos = lexer
  | ";" -> SEMI
  | "(" -> LPAREN
  | ")" -> RPAREN
  | "+" -> PLUS
  | "-" -> MINUS
  | "*" -> MUL
  | "/" -> DIV
  | "extern" ->  EXTERN
  | "def" -> DEF
  | "," -> COMMA
  | "\n" -> new_line lexbuf pos ; alex pos lexbuf 
  |float_literal -> (NUMBER ((u8l|-float_of_string) lexbuf))
  | lident -> IDENT (u8l lexbuf)
  | blank+ -> alex pos lexbuf
  | eof -> if !first then (first := false ;EOF) else raise Ulexing.Error
  | "#" -> comment pos lexbuf
  | _ -> raise Ulexing.Error
and comment pos = lexer 
  | '\n' -> new_line lexbuf pos ; alex pos lexbuf
  | _ -> comment pos lexbuf 


let test filename = Legacy.(
  let revised_parser  = menhir_with_ulex alex main in
  let chan = open_in filename in
  let lexbuf = Ulexing.from_utf8_channel chan in
  handle_ulexing_error revised_parser lexbuf ;
  close_in chan )

let test_lexer filename = Legacy.(
  let chan = open_in filename in 
  let lexbuf = Ulexing.from_utf8_channel chan in 
  let tokens = tokens_of_buf alex lexbuf in 
  Stream.iter (dump |- print_string |- print_newline) tokens
)

let _ = 
  (* test_lexer "sample.py"; *)
  test "sample.py"
