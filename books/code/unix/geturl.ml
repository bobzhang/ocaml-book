open BatPervasives
open Printf
open Util


let default_port = "80"

let host_regexp = {
  regexp = Str.regexp "\\([^/:]*\\)\\(:\\([0-9]+\\)\\)?";
  fields = [1,None; 3, Some default_port];
}
  
let url_regexp = {
  regexp = Str.regexp "http://\\([^/:]*\\(:[0-9]+\\)?\\)\\(/.*\\)";
  fields = [1,None; 3 , None];
}
  
let parse_host host = match regexp_match host_regexp host with
  | Some (host::port::_) -> host, int_of_string port
  | _ -> error host "Ill format host"

(**
   parse_url "http://www.google.com:190/";;
   - : (string * int) * string = (("www.google.com", 190), "/")
*)
let parse_url url = match regexp_match url_regexp url with
  | Some (host::path::_) -> parse_host host, path
  | _ -> error url "Ill formed url"

let send_get url sock = Unix.(
  let s = sprintf "GET %s\r\n" url in
  prerr_endline s ;
  ignore ^$ write sock s 0 ^$ String.length s
)

(**
   it first try proxy,
   if there exists a proxy
   then it sends the url to the proxy
   else it sends the root to the url host name
*)  
let get_url proxy url fdout = Unix.(
  let (hostname, port), path = match proxy with
    | None -> parse_url url
    | Some host -> parse_host host,url  in
  let hostaddr =
    try inet_addr_of_string hostname
    with Failure _ ->
      try (gethostbyname hostname).h_addr_list.(0)
      with Not_found -> error hostname "Host not found" in
  let sock = socket PF_INET SOCK_STREAM 0 in
  finally begin fun _ -> close sock end begin fun _ ->
    connect sock (ADDR_INET (hostaddr,port));
    send_get path sock;
    retransmit sock fdout;
  end ()
)

let geturl () : unit =
  let len = Array.length Sys.argv in
  if len < 2
  then error  "geturl: Usage:" ^$ Sys.argv.(0) ^ " [ proxy [:<port>] ] <url>"
  else
    let proxy,url =
      if len > 2
      then Some Sys.argv.(1), Sys.argv.(2)
      else None, Sys.argv.(1)
    in get_url proxy url Unix.stdout

(*
  when your module has side effects, other module who invoke your library
  will be polluted as well, so next time be careful!!!
  let _ = Unix.handle_unix_error (handle_error  geturl) ()
*)
	
