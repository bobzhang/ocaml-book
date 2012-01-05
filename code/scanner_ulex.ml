(********************************************************************************)
(*	Scanner.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

let regexp alpha = ['a'-'z']
let regexp digit = ['0'-'9']
let regexp hexa = ['0'-'9' 'a'-'f']
let regexp space = [' ' '\t' '\n']
let regexp ident = ['a'-'z' '-'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let regexp variable = ['A'-'Z'] ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']*
let regexp hashed = '#' ['A'-'Z' 'a'-'z' '0'-'9' '-' '_']+
let regexp number = ('-' | '+')? digit+ ('.' digit+)?
let regexp units = alpha+ | '%'
let regexp slc = "//" [^ '\n']+
let regexp nth = ('-' | '+')? digit+ 'n' ('-' | '+') digit+

  
type t = charset_t option * statement_t list
and charset_t = string
and statement_t =
	[ `Import of source_t * medium_t list option
	| `Media of medium_t list * rule_t list
	| `Page of pseudo_page_t option * declaration_t list
	| `Fontface of declaration_t list
	| `Vardecl of Lexing.position * variable_t * expression_t
	| `Rule of rule_t
	]
and source_t =
	[ `String of string
	| `Uri of string
	]
and medium_t = string
and variable_t = string
and rule_t = selector_t list * declaration_t list
and pseudo_page_t = string
and selector_t = simplesel_t * (combinator_t * simplesel_t) list
and simplesel_t =
	[ `Explicit of element_t * qualifier_t list
	| `Generic of qualifier_t * qualifier_t list
	]
and combinator_t =
	[ `Descendant
	| `General_sibling
	| `Adjacent_sibling
	| `Child
	]
and element_t =
	[ `Tag of string
	| `Universal
	]
and qualifier_t =
	[ `Id of string
	| `Class of string
	| `Attr of string * attr_t
	| `Pseudo_class of string
	| `Pseudo_element of string
	| `Sel_func of string * function_t
	]
and function_t =
	[ `Qualified of qualifier_t list
	| `Nth of string
	]
and attr_t =
	[ `Attr_exists
	| `Attr_equals of string
	| `Attr_includes of string
	| `Attr_dashmatch of string
	| `Attr_prefix of string
	| `Attr_suffix of string
	| `Attr_substring of string
	]
and declaration_t = property_t * expression_t * important_t
and property_t = string
and important_t = bool
and expression_t = sentence_t list
and sentence_t = term_t list
and term_t =
	[ `Calc of calc_t
	| `String of string
	| `Ident of string
	| `Uri of string
	| `Hash of string
	| `Term_func of string * expression_t
	| `Slash
	]
and calc_t =
	[ `Varref of Lexing.position * variable_t
	| `Quantity of quantity_t
	| `Sum of Lexing.position * calc_t * calc_t
	| `Sub of Lexing.position * calc_t * calc_t
	| `Mul of Lexing.position * calc_t * calc_t
	| `Div of Lexing.position * calc_t * calc_t
	]
and quantity_t = float * string option



open Lexing 

    
module Lexer ( U : sig
  type token =
      EOF
    | S
    | CHARSET
    | IMPORT
    | MEDIA
    | PAGE
    | FONTFACE
    | OPEN_CURLY
    | CLOSE_CURLY
    | OPEN_ROUND
    | CLOSE_ROUND
    | OPEN_SQUARE
    | CLOSE_SQUARE
    | SEMICOLON
    |COLON
    |DOUBLE_COLON
    | COMMA
    | PERIOD
    | SLASH
    | ASTERISK
    | QUOTIENT
    | PLUS
    | MINUS
    | TILDE
    | GT
    | IMPORTANT
    | ATTR_EQUALS
    | ATTR_INCLUDES
    | ATTR_DASHMATCH
    | ATTR_PREFIX
    | ATTR_SUFFIX
    | ATTR_SUBSTRING
    | URI
    | STRING of string
    | IDENT of string
    | NTH of string
    | HASH of string
    | VAR of string
    | SEL_FUNC of string
    | TERM_FUNC of string
    |  QUANTITY of quantity_t
  exception Error 
end)
  =
struct 
  open U
  exception Error of string
  exception Scanning_error of Lexing.position * string
  exception Syntax_error of Lexing.position
  let menhir_with_ulex main_scanner menhir_parser  lexbuf = 
    let position = ref {
      pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0;
    } in
    let lexer_maker () =
      let ante_position = !position in
      let (nlines, token) = main_scanner 0 lexbuf in
      let _ = position := {
        !position with pos_lnum = !position.pos_lnum + nlines;
      } in
      let post_position = !position in
      (token, ante_position, post_position) in
    let revised_parser =
      MenhirLib.Convert.Simplified.traditional2revised menhir_parser
    in try
         revised_parser lexer_maker
      with
        | Error x -> raise (Scanning_error (!position, x))
        | U.Error	  -> raise (Syntax_error !position)

  let add_lines nlines lexbuf =
	let adder acc el = if el = 10 then acc+1 else acc in
	let lexeme = Ulexing.lexeme lexbuf
	in nlines + (Array.fold_left adder 0 lexeme)


  let trim_lexbuf ?(left = 0) ?(right = 0) lexbuf =
    Ulexing.utf8_sub_lexeme lexbuf left ((Ulexing.lexeme_length lexbuf) - left - right)


  let ltrim_lexbuf lexbuf =
    trim_lexbuf ~left:1 lexbuf


  let rtrim_lexbuf lexbuf =
    trim_lexbuf ~right:1 lexbuf


  let parse_quantity =
    let rex = Pcre.regexp "(?<number>(\\+|-)?[0-9]+(\\.[0-9]+)?)(?<units>%|[A-Za-z]+)?"
    in fun lexbuf ->
      let subs = Pcre.exec ~rex (Ulexing.utf8_lexeme lexbuf) in
      let number = Pcre.get_named_substring rex "number" subs
      and units = try Some (Pcre.get_named_substring rex "units" subs) with Not_found -> None
      in (float_of_string number, units)



  let rec main_scanner nlines = lexer
	| "url("			-> (nlines, URI)
	| ident '('			-> (nlines, TERM_FUNC (rtrim_lexbuf lexbuf))
	| ':' ident '('			-> (nlines, SEL_FUNC (trim_lexbuf ~right:1 ~left:1 lexbuf))
	| nth				-> (nlines, NTH (Ulexing.utf8_lexeme lexbuf))
	| number units?			-> (nlines, QUANTITY (parse_quantity lexbuf))
	| ident				-> (nlines, IDENT (Ulexing.utf8_lexeme lexbuf))
	| variable			-> (nlines, VAR (Ulexing.utf8_lexeme lexbuf))
	| hashed			-> (nlines, HASH (ltrim_lexbuf lexbuf))
	| "@charset" space+		-> (add_lines nlines lexbuf, CHARSET)
	| "@import" space+		-> (add_lines nlines lexbuf, IMPORT)
	| "@media" space+		-> (add_lines nlines lexbuf, MEDIA)
	| "@page" space+		-> (add_lines nlines lexbuf, PAGE)
	| "@font-face" space+		-> (add_lines nlines lexbuf, FONTFACE)
	| space* "!important" space*	-> (nlines, IMPORTANT)
	| space* "/*"			-> comment_scanner nlines lexbuf
	| space* slc space*		-> main_scanner (add_lines nlines lexbuf) lexbuf
	| "="				-> (nlines, ATTR_EQUALS)
	| "~="				-> (nlines, ATTR_INCLUDES)
	| "|="				-> (nlines, ATTR_DASHMATCH)
	| "^="				-> (nlines, ATTR_PREFIX)
	| "$="				-> (nlines, ATTR_SUFFIX)
	| "*="				-> (nlines, ATTR_SUBSTRING)
	| space* "::" space*		-> (add_lines nlines lexbuf, DOUBLE_COLON)
	| space* '*' space*		-> (add_lines nlines lexbuf, ASTERISK)
	| space* 247 space*		-> (add_lines nlines lexbuf, QUOTIENT)	(* 247 is the decimal Unicode codepoint for the division sign *)
	| space* '/' space*		-> (add_lines nlines lexbuf, SLASH)
	| space* '+' space*		-> (add_lines nlines lexbuf, PLUS)
	| space* '-' space*		-> (add_lines nlines lexbuf, MINUS)
	| space* '~' space*		-> (add_lines nlines lexbuf, TILDE)
	| space* '>' space*		-> (add_lines nlines lexbuf, GT)
	| space* '{' space*		-> (add_lines nlines lexbuf, OPEN_CURLY)
	| space* '}' space*		-> (add_lines nlines lexbuf, CLOSE_CURLY)
	| space* ';' space*		-> (add_lines nlines lexbuf, SEMICOLON)
	| space* ':' space*		-> (add_lines nlines lexbuf, COLON)
	| space* ',' space*		-> (add_lines nlines lexbuf, COMMA)
	| space* '(' space*		-> (add_lines nlines lexbuf, OPEN_ROUND)
	| space* ')' space*		-> (add_lines nlines lexbuf, CLOSE_ROUND)
	| '.'				-> (nlines, PERIOD)
	| '['				-> (nlines, OPEN_SQUARE)
	| ']'				-> (nlines, CLOSE_SQUARE)
	| '\''				-> single_string_scanner nlines "" lexbuf
	| '"'				-> double_string_scanner nlines "" lexbuf
	| space				-> (add_lines nlines lexbuf, S)
	| eof				-> (nlines, EOF)
	| _				-> raise (Error (Ulexing.utf8_lexeme lexbuf))


  and single_string_scanner nlines accum = lexer
      | '\''				-> (nlines, STRING accum)
      | _				-> single_string_scanner (add_lines nlines lexbuf) (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


  and double_string_scanner nlines accum = lexer
      | '"'				-> (nlines, STRING accum)
      | _				-> double_string_scanner (add_lines nlines lexbuf) (accum ^ (Ulexing.utf8_lexeme lexbuf)) lexbuf


  and comment_scanner nlines = lexer
      | "*/" space*			-> main_scanner (add_lines nlines lexbuf) lexbuf
      | _				-> comment_scanner (add_lines nlines lexbuf) lexbuf

  let test_parser  lexbuf = menhir_with_ulex main_scanner 
end 
  
