open Sys
open Unix

let beep _ =
  output_char Pervasives.stdout '\007';
  flush Pervasives.stdout;
  ignore (alarm 3)

let _ =
  print_endline "haha";
  let _ = signal sigalrm (Signal_handle beep) in 
  ignore (alarm 1);
  print_endline "sleep beging";
  sleep 99999;
  let c = ref 0 in 
  for i = 1 to 1000000 do
    for j = 1 to i do
      c := !c + j;
    done
  done;
  print_int !c;
  print_endline "sleep end"
  
