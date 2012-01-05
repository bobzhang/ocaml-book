
%{

%}

%token ID EQ LPAREN RPAREN IF ELSE THEN


%nonassoc THEN
%nonassoc ELSE

/*
here we used a nice trick to 
handle such ambiguity. set precedence of THEN, ELSE
both needed
*/

%start stmt 
%type <unit> stmt 

%%


stmt: ID EQ ID {}
  | IF LPAREN ID RPAREN THEN stmt {}
  | IF LPAREN ID RPAREN THEN stmt ELSE stmt {}

  
;
/*
It's tricky here we modify the grammar an unambiguous one 
*/


/*
stmt      : matched {}
          | unmatched {}
          ;

matched   : IF '(' ID ')' matched ELSE matched {}
          ;

unmatched : IF '(' ID ')' matched {}
          | IF '(' ID ')' unmatched {}
          | IF '(' ID ')' matched ELSE unmatched {}
          ;
*/
%%


