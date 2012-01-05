
%token ID COMMA COLON
%token BOGUS /* NEVER LEX */
%start def 
%type <unit>def
%%
def:    param_spec return_spec COMMA {}
        ;
param_spec:  ty {}
        |    name_list COLON ty {}
        ;

/*
return_spec:
             ty {}
        |    name COLON ty {}

        |    ID BOGUS {}   // This rule is never used 
        ;
*/

/* another way to fix the prob */

return_spec : ty {}
        | ID COLON ty {}

ty:        ID {}
        ;
name:        ID {}
        ;
name_list:
             name {}
        |    name COMMA name_list {}
        ;

    

