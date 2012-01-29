let _ =
  use ch = open_in_bin "/bin/sh" in
  print_int (input_binary_int ch);
  print_newline ()

let _ =
  use hdl = Unix.opendir "." in
  try 
    while true do print_endline (Unix.readdir hdl) done 
  with End_of_file -> ()

let _ = 
  use ch = Unix.open_process_in "date" in
  print_endline (input_line ch) 

let _ =
  let m = Mutex.create() in
  use _ = Mutex.lock m; m in
  ignore ()

let _ =
  (* we don't "use" "fd" here, because when "ch" is closed, its underlying "fd"
     will be closed automatically. *)
  let fd = Unix.openfile "test.ml" [Unix.O_RDONLY] 0x644 in
  (* with explicit finializer *)
  use ch = Unix.in_channel_of_descr fd with close_in in
  print_endline (input_line ch)

let dangerous =
  let shout (x, y)  = 
    Printf.printf "Danger! in %dm to %dm\n" x y;
    flush stdout; Unix.sleep 1 in
  (* use pa_use as try_finally *)
  use x,y = 200, 100 with shout in
  use x,y = x + 100, y + 100 with shout in
  use x,y = x + 100, y + 100 with shout in
  failwith "Bomb!"
