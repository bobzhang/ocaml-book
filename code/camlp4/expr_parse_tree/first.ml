expr:
[ ";" LEFTA
  [ seq_expr ]

| "top" RIGHTA
  [ "RE_PCRE"; regexp
  | "REPLACE"; regexp; "->"; sequence
  | "SEARCH"; regexp; "->"; sequence
  | "MAP"; regexp; "->"; sequence
  | "COLLECT"; regexp; "->"; sequence
  | "COLLECTOBJ"; regexp
  | "SPLIT"; regexp
  | "REPLACE_FIRST"; regexp; "->"; sequence
  | "SEARCH_FIRST"; regexp; "->"; sequence
  | "MATCH"; regexp; "->"; sequence
  | "FILTER"; regexp
  | "CAPTURE"; regexp  
  | "function"; OPT "|"; LIST1 regexp_match_case SEP "|"
  (* syntax extension by mikmatch*)    
  | "parser"; OPT parser_ipatt; parser_case_list
  | "parser"; OPT parser_ipatt; parser_case_list
  | "let"; "try"; OPT "rec"; LIST1 let_binding SEP "and"; "in"; sequence;
    "with"; LIST1 lettry_case SEP "|"
 (*  syntax extension mikmatch
     let try a = raise Not_found in a with Not_found -> 24;; *)
  | "let"; LIDENT "view"; UIDENT _; "="; SELF; "in"; sequence
(*   view patterns *)
  | "let"; "module"; a_UIDENT; module_binding0; "in"; expr LEVEL ";"
  | "let"; "open"; module_longident; "in"; expr LEVEL ";"
  | "let"; OPT "rec"; binding; "in"; sequence
  | "if"; SELF; "then"; expr LEVEL "top"; "else"; expr LEVEL "top"
  | "if"; SELF; "then"; expr LEVEL "top"
  | "fun"; fun_def
  | "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list
  | "match"; sequence; "with"; "parser"; OPT parser_ipatt; parser_case_list
  | "match"; sequence; "with"; OPT "|"; LIST1 regexp_match_case SEP "|"
  | "try"; SELF; "with"; OPT "|"; LIST1 regexp_match_case SEP "|"
  | "try"; sequence; "with"; match_case
  | "for"; a_LIDENT; "="; sequence; direction_flag; sequence; "do";
    do_sequence
  | "while"; sequence; "do"; do_sequence
  | "object"; opt_class_self_patt; class_structure; "end" ]
| LEFTA
  [ "EXTEND"; extend_body; "END"
  | "DELETE_RULE"; delete_rule_body; "END"
  | "GDELETE_RULE"
  | "GEXTEND" ]

(* operators *)  
| "," LEFTA
  [ SELF; ","; comma_expr ]

| ":=" NONA
  [ SELF; ":="; expr LEVEL "top"
  | SELF; "<-"; expr LEVEL "top" ]
  
| "||" RIGHTA
   [ SELF; infixop6; SELF ]

| "&&" RIGHTA
  [ SELF; infixop5; SELF ]

| "<" LEFTA
  [ SELF; infix operator (level 0) (comparison operators, and some others);
    SELF ]
| "^" RIGHTA
  [ SELF; infix operator (level 1) (start with '^', '@'); SELF ]
| "::" RIGHTA
  [ SELF; "::"; SELF ]
| "+" LEFTA
  [ SELF; infix operator (level 2) (start with '+', '-'); SELF ]
| "*" LEFTA
  [ SELF; "land"; SELF
  | SELF; "lor"; SELF
  | SELF; "lxor"; SELF
  | SELF; "mod"; SELF
  | SELF; infix operator (level 3) (start with '*', '/', '%'); SELF ]
| "**" RIGHTA
  [ SELF; "asr"; SELF
  | SELF; "lsl"; SELF
  | SELF; "lsr"; SELF
  | SELF; infix operator (level 4) (start with "**") (right assoc); SELF ]
| "unary minus" NONA
  [ "-"; SELF
  | "-."; SELF ]

(* apply *)
| "apply" LEFTA
  [ SELF; SELF
  | "assert"; SELF
  | "lazy"; SELF ]
  
| "label" NONA
  [ "~"; a_LIDENT
  | LABEL _; SELF
  | OPTLABEL _; SELF
  | "?"; a_LIDENT ]
| "." LEFTA
  [ SELF; "."; "("; SELF; ")"
  | SELF; "."; "["; SELF; "]"
  | SELF; "."; "{"; comma_expr; "}"
  | SELF; "."; SELF
  | SELF; "#"; label ]
| "~-" NONA
  [ "!"; SELF
  | prefix operator (start with '!', '?', '~'); SELF ]
| "simple" LEFTA
  [ "false"
  | "true"
  | "{"; TRY [ label_expr_list; "}" ]
  | "{"; TRY [ expr LEVEL "."; "with" ]; label_expr_list; "}"
  | "new"; class_longident
  | QUOTATION _
  | ANTIQUOT (("exp" | "" | "anti"), _)
  | ANTIQUOT ("`bool", _)
  | ANTIQUOT ("tup", _)
  | ANTIQUOT ("seq", _)
  | "`"; a_ident
  | "["; "]"
  | "["; sem_expr_for_list; "]"
  | "[|"; "|]"
  | "[|"; sem_expr; "|]"
  | "{<"; ">}"
  | "{<"; field_expr_list; ">}"
  | "begin"; "end"
  | "begin"; sequence; "end"
  | "("; ")"
  | "("; "module"; module_expr; ")"
  | "("; "module"; module_expr; ":"; package_type; ")"
  | "("; SELF; ";"; ")"
  | "("; SELF; ";"; sequence; ")"
  | "("; SELF; ":"; ctyp; ")"
  | "("; SELF; ":"; ctyp; ":>"; ctyp; ")"
  | "("; SELF; ":>"; ctyp; ")"
  | "("; SELF; ")"
  | stream_begin; stream_end
  | stream_begin; stream_expr_comp_list; stream_end
  | stream_begin; stream_end
  | stream_begin; stream_expr_comp_list; stream_end
  | a_INT
  | a_INT32
  | a_INT64
  | a_NATIVEINT
  | a_FLOAT
  | a_STRING
  | a_CHAR
  | TRY module_longident_dot_lparen; sequence; ")"
  | TRY val_longident ] ]  





















