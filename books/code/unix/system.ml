open BatPervasives
open Printf
open Util


(** Note that the signal changes must be made before the call to
    [fork] is executed because the parent could receive signals
    (sigchld if the child were to finish immediately) before it
    proceeds
*)
let exec_as_system exec args = Unix.(
  let open Sys in 
  let (old_mask, old_int, old_quit) =
    (sigprocmask SIG_BLOCK [sigchld] ,
     signal sigint Signal_ignore,
     signal sigquit Signal_ignore) in
  let reset () = begin
    ignore ^$ signal sigquit old_quit;
    ignore ^$ signal sigint old_int;
    ignore ^$ sigprocmask SIG_SETMASK old_mask;
  end  in
  let call () = match fork() with
    | 0 -> begin
      (* child can not ignore *)
      reset ();
      (try exec  args with _ -> exit 127)
    end
    | k ->
      snd (restart_on_EINTR (waitpid []) k) in
  finally reset call  ()
)


let system cmd = Unix.(
  exec_as_system (execv "/bin/sh")
    [|"/bin/sh"; "-c"; cmd |]
)















