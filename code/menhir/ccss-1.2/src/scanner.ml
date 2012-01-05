(********************************************************************************)
(*	Scanner.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

open Parser


(********************************************************************************)
(**	{1 Exceptions}								*)
(********************************************************************************)

exception Error of string


(********************************************************************************)
(**	{1 Regular expressions}							*)
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


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Auxiliary functions}							*)
(********************************************************************************)

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


(********************************************************************************)
(**	{2 Lexers}								*)
(********************************************************************************)

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

