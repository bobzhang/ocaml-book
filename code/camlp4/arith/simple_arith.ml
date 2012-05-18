open Camlp4.PreCast;
value expr = Gram.Entry.mk "expr";
value expr_eoi = Gram.Entry.mk "expr_eoi";
value parse_expr = Camlp4ext.parse_string_of_entry expr;
value parse_expr_eoi = Camlp4ext.parse_string_of_entry expr_eoi;

EXTEND Gram GLOBAL: expr expr_eoi;
expr_eoi: [[ 
  x = expr ; `EOI -> x
 ]];
expr:
  [ "add"
   [  x = SELF ; "+"; y = SELF -> x + y
   |  x = SELF ; "-"; y = SELF -> x - y]
   |"mult"
   [  x = SELF ; "*"; y = SELF -> x * y
   |  x = SELF ; "/"; y = SELF -> x / y]
   | "pow" RIGHTA
   [ x = SELF ; "**"; y = SELF -> int_of_float (float x ** float y)] 
   |"simple"
   [ "("; x = SELF; ")" -> x
   | `INT(x,_) -> x
  ]];
END;

value _ = print_string "linking.."    
;
