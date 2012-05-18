open Format;
open Camlp4.PreCast;
value expr = Gram.Entry.mk "expr";
value pr = Gram.Entry.print;
value fmt = std_formatter ;
value (|>) x f = f x ;

value p = Gram.parse_string expr (Loc.mk "<string>");
value t = "foo bar baz" ;
open Gram.Entry;


EXTEND Gram GLOBAL: expr ;
    expr : 
      [[ "foo"; f  -> print_endline "first"
       | "foo" ; "bar"; "baz" -> print_endline "second"]
      ]; 
    f : [["bar"; "baz" ]];
END;

p t ; (** second wins*)
clear expr ;

EXTEND Gram GLOBAL: expr;
    expr:
      [["foo";f -> print_endline "first"
      | "foo"; "bar"; "bax" -> print_endline "second"]];
    f: [["bar";"baz"]];
END ;

p t; (** first win*)
clear expr;

EXTEND Gram GLOBAL: expr ;
      expr:
      [["foo";f -> print_endline "first"
      |"foo";"bar"; f -> print_endline "second"]];
      f: [["bar";"baz"]];
END;
p t; (** here peek does not work, it will branch to second and fail*)















