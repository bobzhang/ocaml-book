
%{
  open Printf
  open Lexing 
  let parse_error s = 
    print_endline "impossible happend! panic \n";
    print_endline s ; 
    flush stdout 
%}

%token NEWLINE 
%token LPAREN RPAREN 
%token <float> NUM 
%token PLUS MINUS MULTIPLY DIVIDE CARET 


%left PLUS MINUS MULTIPLY DIVIDE NEG 
%right CARET 

%start input 
%start exp
%type <unit> input 
%type <float> exp 

%% /* rules and actions */


input: /* empty */ {}
    | input line {}
; 

line: NEWLINE {}
    |exp NEWLINE  {printf "\t%.10g\n" $1 ; flush stdout}
;

exp: NUM { $1 }
    | exp PLUS exp		{ $1 +. $3 }
    | exp MINUS exp		{ $1 -. $3 }
    | exp MULTIPLY exp		{ $1 *. $3 }
    | exp DIVIDE exp		{ $1 /. $3 }
    | MINUS exp %prec NEG	{ -. $2 }
    | exp CARET exp		{ $1 ** $3 }
    | LPAREN exp RPAREN	        { $2 }
;

%%

