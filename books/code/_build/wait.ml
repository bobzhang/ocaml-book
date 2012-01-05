open Unix

exception Found

let simple_search cond v =
  try
    for i = 0 to Array.length v -  1 do
      if cond v.(i) then raise Found
    done;
    false
  with Found -> true


(** a naive parallel search *)    
let fork_search cond v =
  let n = Array.length v in
  match fork () with
    |0 ->  (* child *)
      let found = simple_search cond (Array.sub v (n/2) (n-n/2)) in
      exit (if found then 0 else 1)
    |_ ->
      let found = simple_search cond (Array.sub v 0 (n/2)) in
      match wait () with
        | (pid, WEXITED retcode) ->
          (* found by me or child *)
          print_endline "found";
          found || (retcode = 0)
        | (pid, _) ->
          failwith "fork_serach"

let _ =
  fork_search (fun x-> x >10 ) [|1;2;3;3;4;4;11|]
    
