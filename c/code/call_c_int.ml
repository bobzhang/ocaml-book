
type cint 
external cint_of_ml_int : int ->  cint =
    "Cint_of_OCAMLint"
external ml_int_of_cint : cint -> int =
    "OCAMLint_of_Cint"

external cplus : cint -> cint -> cint = "Cplus"

external print_cint : cint -> unit = "printCint"

let _ =
  let a = max_int in
  print_int (a + 1);
  print_newline ();
  flush stdout;
  let b = cint_of_ml_int a  in
  print_cint b;
  print_cint (cplus b (cint_of_ml_int 1))


(** ocamlmktop -custom -o ftop c_int.o call_c_int.cmo
    ./ftop
    then it will execute your code, and load the module

    toplevel, runtime, bytecode, native 
*)
    
