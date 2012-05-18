open Format;
open Camlp4.PreCast;
  
value lambda = Gram.Entry.mk "lambda";
EXTEND Gram GLOBAL: lambda;
  lambda:
    [
     "top" (** str antiquot -> string literal *)
    [ "fun"; x=LIDENT; "->" ; y=SELF ->
         <:expr< `Lambda ( $ <:expr< `Id $str:x$ >> $, $y$ ) >>  ]
    | "app"
    [x=SELF;y=SELF -> <:expr< `App ($x$,$y$ ) >> ]
    |"simple"
      [ "("; x = SELF; ")" -> x
      | x = LIDENT ->  <:expr< `Id $str:x$ >> ]
    ];
END;

Quotutil.of_entry_simple 
  ~name:"lam"
  ~entry:lambda
;
















