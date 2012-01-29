let _ =
  let ((ch as rsrc1)) = open_in_bin "/bin/sh"
  in
    try
      let result = (print_int (input_binary_int ch); print_newline ()) in
      let _ = close_in rsrc1 in result
    with | e -> let _ = close_in rsrc1 in raise e
  
let _ =
  let ((hdl as rsrc2)) = Unix.opendir "."
  in
    try
      let result =
        try while true do print_endline (Unix.readdir hdl) done
        with | End_of_file -> () in
      let _ = Unix.closedir rsrc2 in result
    with | e -> let _ = Unix.closedir rsrc2 in raise e
  
let _ =
  let ((ch as rsrc3)) = Unix.open_process_in "date"
  in
    try
      let result = print_endline (input_line ch) in
      let _ = Unix.close_process_in rsrc3 in result
    with | e -> let _ = Unix.close_process_in rsrc3 in raise e
  
let _ =
  let m = Mutex.create () in
  let ((_ as rsrc4)) = (Mutex.lock m; m)
  in
    try let result = ignore () in let _ = Mutex.unlock rsrc4 in result
    with | e -> let _ = Mutex.unlock rsrc4 in raise e
  
let _ =
  (* we don't "use" "fd" here, because when "ch" is closed, its underlying "fd"
     will be closed automatically. *)
  let fd = Unix.openfile "test.ml" [ Unix.O_RDONLY ] 0x644 in
  (* with explicit finializer *)
  let ((ch as rsrc5)) = Unix.in_channel_of_descr fd
  in
    try
      let result = print_endline (input_line ch) in
      let _ = close_in rsrc5 in result
    with | e -> let _ = close_in rsrc5 in raise e
  
let dangerous =
  let shout (x, y) =
    (Printf.printf "Danger! in %dm to %dm\n" x y; flush stdout; Unix.sleep 1) in
  (* use pa_use as try_finally *)
  let (((x, y) as rsrc8)) = (200, 100)
  in
    try
      let result =
        let (((x, y) as rsrc7)) = ((x + 100), (y + 100))
        in
          try
            let result =
              let (((x, y) as rsrc6)) = ((x + 100), (y + 100))
              in
                try
                  let result = failwith "Bomb!" in
                  let _ = shout rsrc6 in result
                with | e -> let _ = shout rsrc6 in raise e in
            let _ = shout rsrc7 in result
          with | e -> let _ = shout rsrc7 in raise e in
      let _ = shout rsrc8 in result
    with | e -> let _ = shout rsrc8 in raise e
  

