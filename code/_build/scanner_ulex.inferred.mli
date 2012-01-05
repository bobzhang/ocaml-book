val __ulex_table_27 : string
val __ulex_table_15 : string
val __ulex_table_25 : string
val __ulex_table_4 : string
val __ulex_table_23 : string
val __ulex_table_8 : string
val __ulex_table_19 : string
val __ulex_table_3 : string
val __ulex_table_13 : string
val __ulex_table_7 : string
val __ulex_table_26 : string
val __ulex_table_2 : string
val __ulex_table_12 : string
val __ulex_table_21 : string
val __ulex_table_9 : string
val __ulex_table_11 : string
val __ulex_table_14 : string
val __ulex_table_17 : string
val __ulex_table_22 : string
val __ulex_table_28 : string
val __ulex_table_5 : string
val __ulex_table_10 : string
val __ulex_table_18 : string
val __ulex_table_1 : string
val __ulex_table_6 : string
val __ulex_table_16 : string
val __ulex_table_20 : string
val __ulex_table_24 : string
val __ulex_partition_8 : int -> int
val __ulex_partition_22 : int -> int
val __ulex_partition_0 : int -> int
val __ulex_partition_2 : int -> int
val __ulex_partition_23 : int -> int
val __ulex_partition_45 : int -> int
val __ulex_partition_39 : int -> int
val __ulex_partition_17 : int -> int
val __ulex_partition_20 : int -> int
val __ulex_partition_27 : int -> int
val __ulex_partition_34 : int -> int
val __ulex_partition_19 : int -> int
val __ulex_partition_36 : int -> int
val __ulex_partition_16 : int -> int
val __ulex_partition_26 : int -> int
val __ulex_partition_28 : int -> int
val __ulex_partition_29 : int -> int
val __ulex_partition_38 : int -> int
val __ulex_partition_7 : int -> int
val __ulex_partition_42 : int -> int
val __ulex_partition_15 : int -> int
val __ulex_partition_44 : int -> int
val __ulex_partition_3 : int -> int
val __ulex_partition_31 : int -> int
val __ulex_partition_9 : int -> int
val __ulex_partition_13 : int -> int
val __ulex_partition_12 : int -> int
val __ulex_partition_14 : int -> int
val __ulex_partition_25 : int -> int
val __ulex_partition_30 : int -> int
val __ulex_partition_5 : int -> int
val __ulex_partition_33 : int -> int
val __ulex_partition_32 : int -> int
val __ulex_partition_6 : int -> int
val __ulex_partition_43 : int -> int
val __ulex_partition_40 : int -> int
val __ulex_partition_18 : int -> int
val __ulex_partition_41 : int -> int
val __ulex_partition_11 : int -> int
val __ulex_partition_10 : int -> int
val __ulex_partition_24 : int -> int
val __ulex_partition_37 : int -> int
val __ulex_partition_4 : int -> int
val __ulex_partition_21 : int -> int
val __ulex_partition_46 : int -> int
val __ulex_partition_35 : int -> int
val __ulex_partition_1 : 'a -> int
type t = charset_t option * statement_t list
and charset_t = string
and statement_t =
    [ `Fontface of declaration_t list
    | `Import of source_t * medium_t list option
    | `Media of medium_t list * rule_t list
    | `Page of pseudo_page_t option * declaration_t list
    | `Rule of rule_t
    | `Vardecl of Lexing.position * variable_t * expression_t ]
and source_t = [ `String of string | `Uri of string ]
and medium_t = string
and variable_t = string
and rule_t = selector_t list * declaration_t list
and pseudo_page_t = string
and selector_t = simplesel_t * (combinator_t * simplesel_t) list
and simplesel_t =
    [ `Explicit of element_t * qualifier_t list
    | `Generic of qualifier_t * qualifier_t list ]
and combinator_t =
    [ `Adjacent_sibling | `Child | `Descendant | `General_sibling ]
and element_t = [ `Tag of string | `Universal ]
and qualifier_t =
    [ `Attr of string * attr_t
    | `Class of string
    | `Id of string
    | `Pseudo_class of string
    | `Pseudo_element of string
    | `Sel_func of string * function_t ]
and function_t = [ `Nth of string | `Qualified of qualifier_t list ]
and attr_t =
    [ `Attr_dashmatch of string
    | `Attr_equals of string
    | `Attr_exists
    | `Attr_includes of string
    | `Attr_prefix of string
    | `Attr_substring of string
    | `Attr_suffix of string ]
and declaration_t = property_t * expression_t * important_t
and property_t = string
and important_t = bool
and expression_t = sentence_t list
and sentence_t = term_t list
and term_t =
    [ `Calc of calc_t
    | `Hash of string
    | `Ident of string
    | `Slash
    | `String of string
    | `Term_func of string * expression_t
    | `Uri of string ]
and calc_t =
    [ `Div of Lexing.position * calc_t * calc_t
    | `Mul of Lexing.position * calc_t * calc_t
    | `Quantity of quantity_t
    | `Sub of Lexing.position * calc_t * calc_t
    | `Sum of Lexing.position * calc_t * calc_t
    | `Varref of Lexing.position * variable_t ]
and quantity_t = float * string option
module Lexer :
  functor
    (U : sig
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
             | COLON
             | DOUBLE_COLON
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
             | QUANTITY of quantity_t
           exception Error
         end) ->
    sig
      exception Error of string
      exception Scanning_error of Lexing.position * string
      exception Syntax_error of Lexing.position
      val menhir_with_ulex :
        (int -> 'a -> int * 'b) ->
        ('b, 'c) MenhirLib.Convert.traditional -> 'a -> 'c
      val add_lines : int -> Ulexing.lexbuf -> int
      val trim_lexbuf : ?left:int -> ?right:int -> Ulexing.lexbuf -> string
      val ltrim_lexbuf : Ulexing.lexbuf -> string
      val rtrim_lexbuf : Ulexing.lexbuf -> string
      val parse_quantity : Ulexing.lexbuf -> float * string option
      val main_scanner : int -> Ulexing.lexbuf -> int * U.token
      val single_string_scanner :
        int -> string -> Ulexing.lexbuf -> int * U.token
      val double_string_scanner :
        int -> string -> Ulexing.lexbuf -> int * U.token
      val comment_scanner : int -> Ulexing.lexbuf -> int * U.token
      val test_parser :
        'a ->
        (U.token, 'b) MenhirLib.Convert.traditional -> Ulexing.lexbuf -> 'b
    end
