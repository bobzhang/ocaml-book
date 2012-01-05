(********************************************************************************)
(*	Ast.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)

(**	Defines the AST for parsed CSS stylesheets.
*)

(********************************************************************************)
(**	{1 Type definitions}							*)
(********************************************************************************)

(********************************************************************************)
(**	{2 Top-level statements}						*)
(********************************************************************************)

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


(********************************************************************************)
(**	{2 Selectors}								*)
(********************************************************************************)

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


(********************************************************************************)
(**	{2 Declarations}							*)
(********************************************************************************)

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

