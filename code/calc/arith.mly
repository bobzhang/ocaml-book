%{

%}

%token <int>INTEGER
%token PLUS TIMES
%type <int>expr
%start expr
%%

expr : INTEGER {$1}
  | expr expr PLUS {$1 + $2}
  | expr expr TIMES {$1 * $2}

%%


  



  
