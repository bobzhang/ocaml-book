open Match
module G = Mikmatch.Glob

open BatPervasives

let syscall cmd =
  let ic,oc = Unix.open_process cmd in
  let buf = Buffer.create 16 in
  (try
     while true do
       Buffer.add_channel buf ic 1
     done
   with End_of_file  -> ()
  );
  let _ = Unix.close_process (ic,oc) in
  Buffer.contents buf


let _  =
  syscall "ls ../*/*.tex"
  |> flip BatString.nsplit  "\n"
  |> List.filter (BatString.is_empty |- not)
  |> List.iter check 

