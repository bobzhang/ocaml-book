
let rec walk dir = 
  let items =
    try 
      Array.map (fun fn ->
	let path = Filename.concat dir fn in 
	try if Sys.is_directory path then `Dir path else `File path
	with e -> `Error(path,e) ) (Sys.readdir dir)
    with e -> [| `Error (dir,e) |] in 
  Array.fold_right 
    (fun item rest -> match item with 
      |`Dir path -> [< 'item ; walk path; rest >]
      | _ -> [< 'item; rest >]) items [< >]
(**

val walk :
  string ->
  [> `Dir of string | `Error of string * exn | `File of string ] Stream.t

ocamlbuild test_stream.pp.ml

let rec walk dir =
  let items =
    try
      Array.map
        (fun fn ->
           let path = Filename.concat dir fn
           in
             try if Sys.is_directory path then `Dir path else `File path
             with | e -> `Error (path, e))
        (Sys.readdir dir)
    with | e -> [| `Error (dir, e) |]
  in
    Array.fold_right
      (fun item rest ->
         match item with
         | `Dir path ->
             Stream.icons item (Stream.lapp (fun _ -> walk path) rest)
         | _ -> Stream.icons item rest)
      items Stream.sempty
*)
open Batteries
let _ =
  Stream.( walk   "/Users/bobzhang1988"
	     |> take 10 |> iter 
		 (
		   (function `Dir s -> "dir :" ^ s 
		     | `File s -> "file: " ^ s 
		     | `Error (s,e) -> "error: " ^ s ^ " " ^ Printexc.to_string e
		   )  |- print_string |- print_newline)
  );;


    
