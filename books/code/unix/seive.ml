open BatPervasives
open Printf
open Util

let times = 10000
let prime_count = ref 0
let generate k out = Pervasives.(
  let rec aux s =
    if s < k then begin
      output_binary_int out s ;
      aux (s+1);
    end
  in aux 2
)


let read_first_primes inp n = Pervasives.(
  let rec aux acc count =
    if count = 0
    then acc
    else begin
      let i = input_binary_int inp in
      if List.exists (fun n -> i mod n == 0 ) acc
      then begin
	(* print_endline "unhappy"; *)
	aux acc count;
      end 
      else begin
	incr prime_count;
	if !prime_count mod times = 0 then print_newline ^$ print_int ^$ i;
	aux (i::acc) (count - 1);
      end 
    end in
  aux [] n 
)

let rec filter inp = Unix.(
  let open Pervasives in
  (* let _ = print_endline "entering filter" in  *)
  let primes = read_first_primes inp 1000 in
  let fd_in,fd_out = pipe () in
  print_endline "a new process was created!\n";
  match fork() with
    | 0 -> begin
      close fd_out;
      filter (in_channel_of_descr fd_in);
    end
    | pid -> begin
      close fd_in;
      try
	let out = out_channel_of_descr fd_out in 
	while true do
	  let i = input_binary_int inp in
	  if not ^$ List.exists (fun n -> i mod n == 0 ) primes
	  then begin
	    output_binary_int out i;
	    (* print_endline "then"; *)
	  end
	  else begin
	    (* print_endline "else"; *)
	  end 
	done ;
      with End_of_file ->
	begin close fd_out ; end
    end 
)
let print_primes  () = Pervasives.(
  let open Unix in
  let fd_in,fd_out = pipe () in 
  match fork() with
    | 0 -> begin
      close fd_out;
      filter (in_channel_of_descr fd_in);
    end
    | pid -> begin
      close fd_in;
      generate BatInt.max_num (out_channel_of_descr fd_out);
    end 
)

let _ = print_primes ()  











