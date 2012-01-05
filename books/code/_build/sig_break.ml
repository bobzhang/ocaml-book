open Sys

exception Break

let main_loop _ =
  let _ = signal sigint (Signal_handle (fun _ -> raise Break)) in 
  while true do
    try
      print_endline "haha"
    with Break ->
      print_endline "siginit detected";
      Unix.sleep 1000
  done


let _ = Unix.handle_unix_error main_loop ()    
