open BatPervasives
open Printf

let (^$) f x = f x
  
(** [exec_command] will make sure that it will
    exit the program
*)
let exec_command (cmd:string array) = Unix.(
  try
    execvp cmd.(0) cmd 
  with
      Unix_error (err,_,_) -> begin 
	printf "Can not execute %s : %s\n%!"
	  cmd.(0) (error_message err);
	exit 255;
      end 
)

let print_status (program  : string) = let open Unix in function
  | WEXITED (255|0) -> ()
  | WEXITED status ->
    printf "%s exited with code %d\n%!" program status
  | WSIGNALED signal ->
    printf "%s killed by signal %d\n%!" program signal
  | WSTOPPED signal ->
    printf "%s stopped (???)\n%!" program

let split_words =
  Str.(split (regexp "[ \t]+"))
    
let minishell () = Unix.(
  try
    while true do
      let cmd = Pervasives.(input_line stdin) in
      let words = Array.of_list ^$ split_words cmd in
      match fork() with
	| 0 -> exec_command words
	| pid -> begin
	  let _, status = waitpid [] pid in
	  print_status words.(0) status ;
	end 
    done 
  with
      End_of_file -> ()
)
let _ = Unix.handle_unix_error  minishell ()


















