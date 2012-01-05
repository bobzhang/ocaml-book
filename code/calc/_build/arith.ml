type token =
  | INTEGER of (int)
  | PLUS
  | TIMES

open Parsing;;
# 2 "arith.mly"

# 10 "arith.ml"
let yytransl_const = [|
  258 (* PLUS *);
  259 (* TIMES *);
    0|]

let yytransl_block = [|
  257 (* INTEGER *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\001\000\000\000"

let yylen = "\002\000\
\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\001\000\000\000\000\000\002\000\003\000"

let yydgoto = "\002\000\
\005\000"

let yysindex = "\002\000\
\003\255\000\000\000\000\003\255\255\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\005\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\006\000"

let yytablesize = 7
let yytable = "\003\000\
\006\000\007\000\001\000\003\000\004\000\000\000\004\000"

let yycheck = "\001\001\
\002\001\003\001\001\000\001\001\000\000\255\255\001\000"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  "

let yynames_block = "\
  INTEGER\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 11 "arith.mly"
               (_1)
# 64 "arith.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 12 "arith.mly"
                   (_1 + _2)
# 72 "arith.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : int) in
    Obj.repr(
# 13 "arith.mly"
                    (_1 * _2)
# 80 "arith.ml"
               : int))
(* Entry expr *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let expr (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
;;
# 16 "arith.mly"


  



  
# 113 "arith.ml"
