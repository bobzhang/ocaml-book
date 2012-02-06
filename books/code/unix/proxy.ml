open BatPervasives
open Printf
open Util

let get_regexp = {
  regexp = Str.regexp "^[Gg][Ee][Tt][ \t]+\\(.*[^ \t]\\)[ \t]*\r";
  fields = [1,None];
}

let parse_request line = match regexp_match get_regexp line with
  | Some [url] -> url
  | _ -> error line "Ill formed request"

(**
   val proxy_service : Unix.file_descr * int -> unit
   retrive the url and write it to the client_sock
*)
let proxy_service (client_sock,_ ) = Unix.(
  let open Pervasives in 
  let service () =
    try
      let in_chan = in_channel_of_descr client_sock in
      let line = input_line in_chan in
      let url = parse_request line in
      Geturl.get_url None url client_sock
    with
	End_of_file -> error "Ill formed request" "End_of_file encountered" in 
  finally begin fun _ -> close client_sock end (handle_error service) ()
)

let proxy ()   = Unix.(begin
  (* prerr_string "proxy"; *)
  let http_port =
    if Array.length Sys.argv > 1 then
      try int_of_string Sys.argv.(1)
      with Failure _ -> error Sys.argv.(1) "Incorrect port"
    else
      try (getservbyname "http" "tcp").s_port
      with Not_found -> error "http" "Unknown service"  in
  let _ = prerr_string (sprintf "http port : %d\n" http_port) in 
  let treat_connection   =  fork_treatment  proxy_service in 
  let addr = ADDR_INET(inet_addr_any, http_port) in
  begin
    prerr_string "Setup service\n";
    tcp_server treat_connection addr;
  end
end 
)
  
let _ = begin
  Unix.handle_unix_error (handle_error proxy) ();
end 


















