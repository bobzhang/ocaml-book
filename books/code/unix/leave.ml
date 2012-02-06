
(**
   spawn a child process, it self dies directly,
   therefore it could return control to the shell
*)
open BatPervasives
open Printf
let _ = Unix.(

  let hh,mm =
    try
      (int_of_string (String.sub Sys.argv.(1) 0 2 ),
       int_of_string (String.sub Sys.argv.(1) 2 2))
    with Failure _ -> begin prerr_string "Usage: dddd"; exit 2  end
  in
  let {tm_hour; tm_min} = time () |> localtime in
  let to_slepp = 3600 * (hh - tm_hour ) + 60 *(mm - tm_min) in begin 
    printf "%d ... \n" to_slepp;
    if fork () <> 0
    then exit 0
    else begin
      sleep to_slepp;
      print_endline "\007\007Time to leave!";
      exit 0
    end
  end 
)
