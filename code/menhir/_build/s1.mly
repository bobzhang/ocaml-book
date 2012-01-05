
%token <int>INT
%token < int->int->int > PLUS
%token < int->int->int > MINUS
%token < int->int->int > TIMES
%token < int->int->int > DIV
%token LPAREN RPAREN
%token EOL


%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start<int>main


%%
main:
  | e = expr EOL {e}
  ;
  
expr :
  | i = INT {i}
  | l = expr  f = op  r = expr {
    f l  r
  }
  | MINUS e=expr %prec UMINUS {- e}
  | e = delimited(LPAREN, expr, RPAREN) {e}
  ;
  

%inline op:
  | PLUS {(+)}
  | MINUS {(-)}
  | TIMES {( * )}
  | DIV  { ( / )}
    ;


%%

