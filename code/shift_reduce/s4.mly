
%{

%}

%token PLUS TIMES ID LPAREN RPAREN


%left PLUS 
%left TIMES /* weird ocamlyacc can not detect typo TIMEs */ 

/*
here we add assiocaitivity and precedence
*/

%start expr 
%type <unit> expr 


%%

expr: expr PLUS expr {} 
  | expr TIMES expr {}
  | ID {}
  | LPAREN expr RPAREN {}
;
  

