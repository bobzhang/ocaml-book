open Camlp4.PreCast
  
let expression = Gram.Entry.mk "expression" 

let _ =
  EXTEND Gram 
    GLOBAL: expression ; 
  expression : [
     "add" LEFTA 
   [ x = SELF ; "+" ; y = SELF ->  x +  y 
   | x = SELF ; "-" ; y = SELF ->  x -  y]
  | "mult" LEFTA 
   [ x = SELF ; "*" ; y = SELF -> x * y
   | x = SELF ; "/" ; y = SELF -> x / y]
  | "pow" RIGHTA 
   [ x = SELF ; "**" ; y = SELF -> int_of_float (float x ** float y) ]
  | "simple" NONA 
   [ x = INT -> int_of_string x 
   | "(" ; x = SELF ; ")" -> x ]   
  ] ; 
  END
    
let  _ =
  Printf.printf "%d" (
    Gram.parse_string 
      expression 
      (Loc.mk "<string>" ) "3 +  ((4 - 2) + 28 * 3 ** 2) + (4 / 2)" );
    (* (read_line ()) ;  *)
