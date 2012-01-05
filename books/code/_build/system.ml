
open Batteries
open Sys
open Unix


let rec restart_on_ENTR f x =
  try
    f x 
  with
      Unix_error(EINTR,_,_)-> restart_on_ENTR f x 

let exec_as_system exec args =
  let old_mask = sigprocmask SIG_BLOCK [sigchld] in
  let old_int = signal sigint Signal_ignore in
  let old_quit = signal sigquit Signal_ignore in
  let reset () =
    ignore (signal sigint old_int);
    ignore (signal sigquit old_quit);
    ignore (sigprocmask SIG_SETMASK old_mask) in
  let system_call () = match fork () with
    | 0 ->
      reset ();
      (try exec args with _ -> exit 127 )
    | k ->
      snd (restart_on_ENTR (waitpid []) k) in
  finally reset system_call ()

let system cmd =
  exec_as_system (execv "/bin/sh") [| "/bin/sh"; "-c"; cmd |]
