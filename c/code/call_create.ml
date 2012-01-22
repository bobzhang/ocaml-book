
type c_int_array
external cia_create : int -> c_int_array = "create"
external cia_get : int -> c_int_array -> int = "get"
external cia_put : int -> int -> c_int_array -> unit = "put"

let _ = 
  let tbl = cia_create 10
  and tbl2 = cia_create 10
  in
  for i = 0 to 9 do
    cia_put i (i*2) tbl;
  done;
  for i = 0 to 9 do
    print_int (cia_get i tbl);
    print_string " ";
  done ;
  print_newline();
  for i = 0 to 9 do
    cia_put (9-i) (cia_get i tbl) tbl2
  done ;
  for i = 0 to 9 do
    print_int (cia_get i tbl2);
    print_string " ";
  done;
  flush stdout

let _ =
  print_newline();
  Gc.full_major();
  flush stdout
    
    

