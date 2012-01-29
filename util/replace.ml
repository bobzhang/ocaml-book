(* let files = Netglob.glob ~base_dir (`String "./*.tex") *)

open BatPervasives

let transform_by_line f fn = begin 
  fn
  |> BatFile.lines_of
  |> BatEnum.map f
  |> BatFile.write_lines (fn^".bak");
  Sys.rename fn (fn^".bak~");
  Sys.rename (fn^".bak") fn;
end 

let transform fn =
  transform_by_line
    (REPLACE ("bluecode" | "redcode" | "alternatecode") -> "ocamlcode") fn

let tf x  =
 match x with
   | (RE (_* "inputminted[" as before)  (_* Lazy as i)  ("]" _* as after ))
      -> before ^ "fontsize=\\scriptsize, " ^ i ^ after
   | (RE (_* "inputminted" as before) (_* as after))
     -> before ^ "[fontsize=\\scriptsize, ]" ^ after 
   | _ -> x
     
open Printf
(** KISS dir filter is enough
*)
let rec walk dir ~dir_pred:dir_pred
      = 
  let items =
    try 
      Array.map (
	fun fn ->
	    (let path = Filename.concat dir fn in 
	     try
	       if Sys.is_directory path
	       then `Dir path
	       else `File path
	     with e -> `Error(path,e))) (Sys.readdir dir)
    with e -> [| `Error (dir,e) |] in 
  Array.fold_right 
    (fun item rest -> match item with 
      |`Dir path ->
	if dir_pred path
	then [< walk path ~dir_pred; rest >]
	else  rest
      |`Error (path,e) -> begin 
	prerr_string (sprintf "Path  %s : %s \n" path (Printexc.to_string e));
	rest 
      end 
      | `File p -> [< 'p; rest >])
    items [< >];;

let _ =
  let dfs = walk "../../"
    ~dir_pred:(function | (RE _* "_build") -> false | _ -> true) in 
  dfs
  |> BatStream.filter (fun s -> BatString.ends_with s ".tex")
  |> Stream.iter (fun s -> begin 
    print_string (s ^"\t processing\n");
    transform_by_line tf s
    end)

