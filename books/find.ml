
open Batteries_uni
exception Hidden of exn 

(** add a tag to exn *)
let hide_exn f x = try f x with exn -> raise (Hidden exn)

(** strip the tag of exn *)
let reveal_exn f x = try f x with Hidden exn -> raise exn 

(* let find on_error on_path ?(follow=false) depth roots =  *)
(*   let stat = if follow then Unix.stat else Unix.lstat in  *)
(*   let rec find_rec depth visiting filename =  *)
(*     try  *)
(*       let {Unix.st_dev, st_ino, st_kind} = stat filename in  *)
(*       let continue = hide_exn (on_path filename) infos in  *)
(*       (\* a directory is identified by the id pair *\) *)
(*       let id = st_dev, st_ino in  *)
(*       if st_kind = S_DIR && depth > 0 && continue &&  *)
(*         (\* avoid cycle when it's symbol link *\) *)
(*         (not follow || not (List.mem id visiting))  *)
(*       then  *)
(*         let rec iter_dir f root =  *)
(*           let files = Sys.readdir root in  *)
(*           Array.iter f files  *)
(*         in  *)
(*         let process_child child = let open Filename in  *)
(*           if (child <> current_dir_name &&  *)
(*               (\* remove the case "." or ".." *\) *)
(*               child <> parent_dir_name ) then  *)
(*             let child_name = concat filename child in  *)
(*             let visiting =  *)
(*               (\* we only keep track when follow is specified *\) *)
(*               if follow then id :: visiting else visiting in  *)
(*             find_rec (depth - 1) visiting child_name  in  *)
(*         iter_dir process_child filename                           *)
(*     with                                *)
(*         Unix_error (e,b,c) -> hide_exn on_error (e,b,c)  *)
(*     in  *)
(*   reveal_exn (List.iter (find_rec depth [] )) roots        *)


let buffer_size = 8192 
let buffer = String.create buffer_size 

let file_copy input output = Unix.(
  let fd_in = openfile input [O_RDONLY] 0 in 
  let fd_out = openfile output [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in 
  let rec copy_loop () = match read fd_in buffer 0 buffer_size with 
    |0 -> ()
    |r -> write fd_out buffer 0 r |> ignore; copy_loop () in 
  copy_loop ();
  close fd_in ; 
  close fd_out 
)


let copy () = 
  if Array.length Sys.argv = 3 then begin 
    file_copy Sys.argv.(1) Sys.argv.(2)
  end 
  else begin 
    prerr_endline 
      ("Usage: " ^ Sys.argv.(0) ^ "<input_file> <output_file>"); 
    exit 1 
  end 

let _  = Unix.handle_unix_error copy () 


