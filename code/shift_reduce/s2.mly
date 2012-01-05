

%{
%}


%token OPAREN CPAREN ID SEMIC DOT INT EQUAL 

%start stmt 
%type <int> stmt 

%%
stmt: methodcall {0} | arrayasgn {0}
; 

/*
previous 
methodcall: target OPAREN CPAREN SEMIC {0}
; 
target:  ID DOT ID {0} |ID {0}
;

our strategy was to remove the "extraneous" non-terminal in the 
methodcall production, by moving one of the right-hand sides of target 
to the methodcall production 

*/

methodcall: target OPAREN CPAREN SEMIC {0} | ID OPAREN CPAREN SEMIC {0}
; 
target:  ID DOT ID {0}
;
arrayasgn: ID OPAREN  INT CPAREN EQUAL INT SEMIC {0}
;


           

