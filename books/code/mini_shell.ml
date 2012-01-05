
open Unix
open Printf
open Pcre

let split_words s =  Array.of_list ((SPLIT space+) s )

let exec_command cmd =
  try execvp cmd.(0) cmd
  with Unix_error(err,_,_)->
    printf "Can not execute %s : %s\n%!"
      cmd.(0) (error_message err);
    exit 255

let print_status program status = match status with
  |WEXITED 255 -> ()
  |WEXITED status ->
    printf "%s exited with code %d\n%!" program status
  |WSIGNALED signal ->
    printf "%s killed by signal %d\n%!" program signal
  |WSTOPPED signal ->
    printf "%s stoped (???)\n%!" program

let minishell () =
  try
    while true do
      let cmd = input_line Pervasives.stdin in
      let words = split_words cmd in
      match fork () with 
        | 0 -> exec_command words (* child *)
        | pid_son ->
          let pid,status = wait () in
          print_status "Program" status
    done
  with End_of_file -> ()

let _ = handle_unix_error minishell ()    
  
