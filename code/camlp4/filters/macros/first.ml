DEFINE PI = 3.1415926
DEFINE F(x) = x +. x
  
let _  =
  IFDEF DEBUG THEN   
    print_float PI
  ENDIF;
  print_float (F(PI));
  print_endline "";
  IFDEF DEBUG THEN
    print_endline "DEBUG"
  ELSE
    print_endline "RELEASE"
  END

