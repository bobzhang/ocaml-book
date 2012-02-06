open BatPervasives
open Printf
open Util

let compose  ()  = Unix.(
  let len = Array.length Sys.argv - 1 in begin
    (* prerr_string ^$ sprintf "len %d\n" len ; *)
    for i = 1 to len - 1 do
      let fd_in, fd_out = pipe () in
      match fork () with
	| 0 -> begin
	  dup2 fd_out ^$ stdout;
	  close fd_out;
	  close fd_in;
	  execv "/bin/sh" [|"/bin/sh"; "-c"; Sys.argv.(i)|];
	end 
	| pid -> begin
	  dup2 fd_in ^$ stdin; (** inherited later *)
	  close fd_out;
	  close fd_in;
	end 
    done;
    match fork () with
      | 0 -> begin
	execv "/bin/sh" [| "/bin/sh";"-c";Sys.argv.(len) |];
      end
      | pid -> begin
	close stdout;
	let rec wait_for_children retcode =
	  try begin
	    match wait () with
	      | (_, WEXITED n) -> wait_for_children (retcode lor n)
	      | (_,_) -> wait_for_children 127
	  end
	  with Unix_error(ECHILD,_,_) -> retcode in begin
	    (** close stdout; you can not close stdout here, since
		your previous fork may be very slow, and does not
		inherit it yet, and dup2 a closed stdout will emit an
		error*)
	    exit (wait_for_children 0);
	  end 
      end 
  end
)

let _ = Unix.handle_unix_error compose ()  


















