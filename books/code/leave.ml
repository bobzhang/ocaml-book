

open Sys
open Unix

let leave () =
  let hh,mm = (int_of_string (String.sub Sys.argv.(1) 0 2),
               int_of_string (String.sub Sys.argv.(1) 2 2)) in
  let now = localtime(time ()) in
  let delay = (hh - now.tm_hour) * 3600 + (mm-now.tm_min) * 60 in
  if delay <= 0 then begin
    print_endline "Hey! That time has alread passed";
    exit 0
  end ;
  if fork() <> 0 then exit 0 ; 
  (** parent process is dead, child process
      as a background process *)
  sleep delay ;
  print_endline "\007\007\007Time to leave";
  exit 0

let _ = handle_unix_error leave ()    
