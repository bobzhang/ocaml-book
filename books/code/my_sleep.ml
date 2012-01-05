
open Unix 
open Sys 
open Batteries

let my_sleep s =
  let old_alarm = signal sigalrm (Singal_handle (fun s -> ())) in
  let old_mask = sigprocmask SIG_UNBLOCK [sigalrm] in (* UNBLOCK *)

  let _ = alarm s in (* set *)
  let new_mask = List.filter (fu x -> x <> sigalrm) old_mask in
  sigsuspend new_mask; (* temporarily suspends the signals in the list l*)
  let _ = alarm 0 in
  ignore (singal sigalrm old_alarm); (* reset *)
  ignore (sigprocmask SIG_SETMASK old_mask) (* reset *)


    
