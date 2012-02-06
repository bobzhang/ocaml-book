open BatPervasives
open Printf
open Util

let sleep  s = Unix.(
  let open Sys in
  let old_alarm, old_mask =
    signal sigalrm (Signal_handle (fun s -> ())),
    (* make sure it is not masked *)
    sigprocmask SIG_UNBLOCK [sigalrm] in
  let _ = alarm s in
  let new_mask =
    List.filter (fun x -> x <> sigalrm) old_mask in
  begin 
    sigsuspend new_mask;
    ignore ^$ alarm 0 ;
    ignore ^$ signal sigalrm old_alarm;
    ignore ^$ sigprocmask SIG_SETMASK old_mask;
  end 
)

















