open BatPervasives

let (^$) f x = f x
  
let rec restart_on_EINTR f x = Unix.(
  try
    f x
  with
      Unix_error (EINTR,_,_) -> restart_on_EINTR f x
)	


(** recover any dead children and repeats until either there are only
    live children (zero is returned instead of a child id) or there
    are no children (ECHILD exception)*)
let free_children _ = Unix.(
  try
    while fst (waitpid [WNOHANG] (-1)) > 0 do
      ()
    done ;
  with
      Unix_error (ECHILD,_,_) -> ()
)

(* val BatIO.to_string : (string output -> 'a -> unit) -> 'a -> string *)
let print_protocol_entry output = Unix.( function
  | {p_name; p_aliases; p_proto } -> begin 
    BatPrintf.fprintf output
      "{p_name: %a; p_aliases: %a; p_proto: %a}"
      BatString.print p_name
      (BatArray.print BatString.print) p_aliases
      BatInt.print p_proto
  end 
)

let print_socket_type output = Unix.(fun x ->
  (match x with 
   | SOCK_STREAM ->  "SOCK_STREAM"
   | SOCK_DGRAM ->  "SOCK_DGRAM"
   | SOCK_RAW -> "SOCK_RAW"
   | SOCK_SEQPACKET -> "SOCK_SEQPACKET") |> BatString.print output
)

let print_socket_domain output = Unix.(fun x ->
  (match x with
    | PF_UNIX -> "PF_UNIX"
    | PF_INET -> "PF_INET"
    | PF_INET6 -> "PF_INET6") |> BatString.print output
)

let print_inet_addr output =
  Unix.string_of_inet_addr |- BatString.print output
      
let print_sockaddr output = Unix.(function
  | ADDR_UNIX s -> begin
    BatPrintf.fprintf output "ADDR_UNIX ( %s )"  s;
  end
  | ADDR_INET (inet_addr,i) -> begin
    BatPrintf.fprintf output "ADDR_INET ( %a , port : %d)"
      print_inet_addr inet_addr
      i
  end 
)

let print_host_entry output = Unix.( function
  | {h_name; h_aliases; h_addrtype; h_addr_list} -> begin
    BatPrintf.fprintf output
      "{h_name: %a; h_aliases: %a; h_addrtype: %a; h_addrlist: %a}"
      BatString.print h_name
      (BatArray.print BatString.print) h_aliases
      print_socket_domain h_addrtype
      (BatArray.print print_inet_addr) h_addr_list
  end
)

let print_service_entry output = Unix.( function
  | {s_name; s_aliases; s_port; s_proto} -> begin
    BatPrintf.fprintf output
      "{s_name: %a; s_aliases: %a; s_port: %a; s_proto: %a}"
      BatString.print s_name
      (BatArray.print BatString.print) s_aliases
      BatInt.print s_port
      BatString.print s_proto ;
  end 
)
  

(** maybe interrupted by a signal *)
let retransmit fdin fdout = Unix.(
  let buffer_size = 4_096 in
  let buffer = String.create buffer_size in
  let rec copy () = match read fdin buffer 0 buffer_size with
    | 0 -> ()
    | n -> begin
      ignore ^$ write fdout buffer 0 n ;
      copy ();
    end  in
  copy ()
)
  
let install_tcp_server_socket addr = Unix.(
  let sock_server = socket PF_INET SOCK_STREAM 0 in
  try begin 
    bind sock_server addr ;
    listen sock_server 10 ; (**  hold at most 10 *)
    sock_server 
  end
  with e -> begin
    close sock_server;
    raise e;
  end
)

(** [treat_connection] receives [sock_server] and [client_info]
    as argument, it should handle EPIPE exception
*)  
let tcp_server treat_connection addr = Unix.(
  let _ = ignore ^$ Sys.(signal sigpipe Signal_ignore) in 
  let sock_server = install_tcp_server_socket addr in
  while true do
    let client_info = restart_on_EINTR accept sock_server in
    treat_connection sock_server client_info
  (** needs sock_server for inheritence when fork *)
  done 
)

(** client_sock is duplicated, parent should close it as well.
    sock_server is duplicated, child should close it 
*)
let fork_treatment service sock_server (client_sock,_ as client) = Unix.(
  let f () = match fork () with
    | 0 -> begin 
      close sock_server; (** don't need it any more*)
      service client ;
      exit 0;
    end
    | _ -> begin
      (); (** continue parent's initial work *)
    end
  in
  finally (fun _ -> close client_sock) f ()
)
  
let co_treatment service sock_server (client_sock, _ as client) = Thread.(
  try create (service client)
  with exn -> begin
    Unix.close client_sock;
    raise exn ;
  end 
)

type regexp = {
  regexp : Str.regexp;
  fields : (int * string option) list;
}

(** Str module is stateful *)
let regexp_match r string =
  let get ((pos : int) , default) =
    try Str.matched_group pos string
    with Not_found ->
      match default with Some s -> s | _ -> raise Not_found in
  try
    if Str.string_match r.regexp string 0
    then Some (List.map get r.fields)
    else None 
  with Not_found -> None

exception Error of string

let error err mes = raise ^$ Error ( err ^ ": "  ^ mes)
let handle_error f x =
  try f x
  with Error err -> begin
    prerr_endline err;
    exit 2;
  end 
    
