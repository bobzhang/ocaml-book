%{
  open Printf
  open Lexing 
  let parse_error s = 
    print_endline "impossible happend! panic \n";
    print_endline s ; 
    flush stdout 
  let var_table = Hashtbl.create 16 
%}


%token NEWLINE 
%token LPAREN RPAREN EQ
%token <float> NUM 
%token PLUS MINUS MULTIPLY DIVIDE CARET 
%token <string> VAR 
%token <float->float>FNCT /* built in function */

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG 

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
    |error NEWLINE {}
;

exp: NUM { $1 }
    | VAR 
        {try Hashtbl.find var_table $1 
          with Not_found -> 
            printf "unbound value '%s'\n" $1;
            0.0
        }
    | VAR EQ exp 
        {Hashtbl.replace var_table $1 $3; $3}
    | FNCT LPAREN exp RPAREN
        { $1 $3 }
    | exp PLUS exp		{ $1 +. $3 }
    | exp MINUS exp		{ $1 -. $3 }
    | exp MULTIPLY exp		{ $1 *. $3 }
    | exp DIVIDE exp		
        { if $3 <> 0. then $1 /. $3 
          else 
            Parsing.(
              let start_pos = rhs_start_pos 3 in 
              let end_pos = rhs_end_pos 3 in 
              printf "%d.%d --- %d.%d: dbz"
                start_pos.pos_lnum (start_pos.pos_cnum -start_pos.pos_bol)
                end_pos.pos_lnum (end_pos.pos_cnum - end_pos.pos_bol); 
              1.0
            )}
    | MINUS exp %prec NEG	{ -. $2 }
    | exp CARET exp		{ $1 ** $3 }
    | LPAREN exp RPAREN	        { $2 }
;

%%



(** lexer file *)
{
  open Rpcalc
  open Printf
  let first = ref true
}


let digit = ['0'-'9']
let id = ['a'-'z']+
rule token = parse 
  |[' ' '\t' ] {token lexbuf}
  |'\n' {Lexing.new_line lexbuf ; NEWLINE}
  | (digit+ | "." digit+ | digit+ "." digit*) as num 
      {NUM (float_of_string num)}
  |'+' {PLUS}
  |'-' {MINUS}
  |'*' {MULTIPLY}
  |'/' {DIVIDE}
  |'^' {CARET}
  |'(' {LPAREN}
  |')' {RPAREN}
  |"sin" {FNCT(sin)}
  |"cos" {FNCT(cos) }
  |id as x {VAR x}
  |'=' {EQ}
  |_ as c  {printf "unrecognized char %c" c ; token lexbuf}
  |eof {
    if !first then begin first := false; NEWLINE end 
    else raise End_of_file }


{
  let main ()  = 
    let file = Sys.argv.(1) in 
    let chan = open_in file in 
    try 
      let lexbuf = Lexing.from_channel chan in 
      while true do 
        Rpcalc.input token lexbuf 
      done 
    with End_of_file -> close_in chan 

 let _ = Printexc.print main ()

}

