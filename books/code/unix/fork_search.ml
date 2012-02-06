
open BatPervasives

exception Found
  
let simple_find f arg s len =
  try begin 
    for i = s to s + len - 1  do 
      if f arg.(i)
      then raise Found
    done ;
    false;
  end
  with Found -> true

(** parent communicate child via exit code child process signal by the
    exit code, and the parent process to check it out
*)
let fork_search f arg  = Unix.(
  let n = Array.length arg in
  match fork () with
    | 0 -> (** child *)
      exit(
	if simple_find f arg 0 (n/2)
	then 0
	else 1 
      )
    | pid -> begin 
      let ret = simple_find f arg (n/2) ((n+1)/2) in
      match wait () with
	| _, WEXITED i -> ret || i == 0
	| _, _  -> failwith "fork_search";
    end 
)

  
