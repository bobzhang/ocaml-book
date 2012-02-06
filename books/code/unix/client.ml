open BatPervasives
open Printf
open Util



let client () = Unix.( begin 
  if Array.length Sys.argv < 3 then begin
    prerr_endline "Usage: client <host> <port>";
    exit 2 
  end ;
  let server_name = Sys.argv.(1)
  and port_number = int_of_string Sys.argv.(2) in
  let server_addr =
    try
      (gethostbyname server_name).h_addr_list.(0)
    with
	Not_found -> begin
	  prerr_endline ^$ server_name ^ ": Host not found";
	  exit 2 ;
	end in
  let sock = socket PF_INET SOCK_STREAM 0 in
  connect sock (ADDR_INET (server_addr, port_number));
  match fork() with
    | 0 -> begin 
      retransmit stdin sock;
      shutdown sock SHUTDOWN_SEND;
      exit 0 ;
    end
    |_ -> begin 
      retransmit sock stdout;
      close stdout;
      wait ();
    end 
end)

let _ = Unix.handle_unix_error client ()  
















