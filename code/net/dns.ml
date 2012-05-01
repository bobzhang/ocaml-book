open Format
open Unix

(** dns
    gethostbyname : string -> Unix.host_entry 
 *)  
let look name  =
  try
    let addr = gethostbyname name in
    addr.h_addr_list |> Array.iter
	(fun addr ->
	  addr
        |> string_of_inet_addr
	|> print_endline)
  with 
    Not_found -> printf "can not resolve %s\n" name


(**
   gethostbyname
   gethostbyaddr
   inet_addr_of_string
 *)















