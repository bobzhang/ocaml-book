type camlp4_token = Sig.camlp4_token ==
  [ KEYWORD       of string 
  | SYMBOL        of string (* *, +, +++*%, %#@... *)
  | LIDENT        of string (* lower case identifier *)
  | UIDENT        of string (* upper case identifier *)
  | ESCAPED_IDENT of string (* ( * ), ( ++##> ), ( foo ) *)
  | INT           of int and string  (* `INT(i,is)  42, 0xa0, 0XffFFff, 0b1010101, 0O644, 0o644 *)
  | INT32         of int32 and string (* 42l, 0xa0l... *)
  | INT64         of int64 and string (* 42L, 0xa0L... *)
  | NATIVEINT     of nativeint and string (* 42n, 0xa0n... *)
  | FLOAT         of float and string (* 42.5, 1.0, 2.4e32 *)
  | CHAR          of char and string  (* with escaping *)
  | STRING        of string and string (* with escaping *)
  | LABEL         of string (* *)
  | OPTLABEL      of string
  | QUOTATION     of Sig.quotation (* << foo >> <:quot_name< bar >> <@loc_name<bar>>
     type quotation ={ q_name : string; q_loc : string;
     q_shift : int; q_contents : string; } *)
  | ANTIQUOT      of string and string    (* $foo$  $anti_name:foo$ $`anti_name:foo$ *)
  | COMMENT       of string               
  | BLANKS        of string               (* some non newline blanks *)
  | NEWLINE                               (* interesting *)
  | LINE_DIRECTIVE of int and option string  (* #line 42, #foo "string" *)
  | EOI ];
