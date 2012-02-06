open BatPervasives
open Printf
open Util

let server () = Unix.( 
  if Array.length Sys.argv < 3 then begin
    prerr_endline ^$ "Usage: server port cmd arg1 .. "
  end ;
  let port = int_of_string Sys.argv.(1) in
  let args = Array.sub Sys.argv 2 (Array.length Sys.argv - 2) in
  let host = (gethostbyname (gethostname())).h_addr_list.(0) in
  let addr = ADDR_INET(host,port) in
  let treat sock_server (client_sock, client_addr as client) =   begin
    (match client_addr with
      | ADDR_INET(caller,_) -> begin
	prerr_endline ^$ "Connection from " ^ string_of_inet_addr caller;
      end
      | ADDR_UNIX _ -> begin
	prerr_endline ^$ "Connection from the Unix domain (???)";
      end );
    let service (s,_) = begin
      dup2 s ^$ stdin;
      dup2 s ^$ stdout;
      dup2 s ^$ stderr;
      close s ;
      execvp args.(0) args;
    end  in 
    fork_treatment service sock_server client 
  end in
  tcp_server treat addr
)

let _ = Unix.handle_unix_error server ()  


















