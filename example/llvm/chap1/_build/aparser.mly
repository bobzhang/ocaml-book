
%{
  open Ast 
%}



%token DEF EXTERN PLUS MINUS MUL DIV LPAREN RPAREN SEMI COMMA EOF
%token <float> NUMBER
%token <string> IDENT



%left PLUS MINUS
%left MUL DIV


%start main
%type <unit> main
%type <Ast.expr> expr 
%%

main: 
| list(pair(phrase,SEMI)) EOF {()}
;
phrase:
|expr {(print_endline "expr";)}
|proto{(print_endline "proto";)}
|func {(print_endline "func")}
;
expr:
| l = expr  c = op r = expr {
  Binary (c,l,r)}

| f = NUMBER {Number f}

| f = IDENT   
    args = delimited(LPAREN,separated_list( COMMA, expr),RPAREN) 
    {Call(f, Array.(of_list args)) }
| f = IDENT {Variable f}
| LPAREN e=expr RPAREN {e}
;


proto:
|EXTERN f=IDENT 
    args = delimited(LPAREN,separated_list(COMMA,IDENT),RPAREN) 
    {Prototype(f, Array.of_list args)}
;

func:
| DEF f = IDENT
    args = delimited(LPAREN,separated_list(COMMA,IDENT),RPAREN) 
    e = expr {
      Function((Prototype(f,Array.of_list args)), e)
    }
;

%inline op :
| PLUS {'+'}
| MINUS {'-'}
| MUL {'*'}
| DIV {'/'}
;


%%


  



  
