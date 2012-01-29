(* Example 1: val read_files: string list -> string list

   Given a list of file names, concat the content of files. If a file doesn't
   exist, simply return [] as it content; otherwise read the content of
   the file as a list of string.
*)


(* Original syntax: typical naive solution, not tail recursive *)
let rec read_files names =
  let rec read_one_file ch =
    try
      let line = input_line ch in
      line :: read_one_file ch             (* Not tail recursive *)
    with End_of_file -> [] in              (* Not tail recursive *)
  match names with
  | [] -> []
  | h :: t ->
      let hs =
        try
          let ch = open_in h in
          let s = read_one_file ch in
          let _  = close_in in
          s
        with Sys_error _ -> [] in
      let rest = read_files t in            (* Not tail recursive *)
      hs @ rest                             (* Not tail recursive *)
 

(* Original syntax: tail recursive version *)
let rec read_files acc names =
  let rec read_one_file lines ch =
    let line_opt =
      try Some (input_line ch)
      with End_of_file -> None in
    match line_opt with
    | Some line -> read_one_file (line :: lines) ch
    | None -> lines in
  match names with
   | [] -> List.rev acc
   | h :: t -> 
       let hs =
         try
           let ch = open_in h in
           let s = read_one_file [] ch in
           let _ = close_in in
           s
         with Sys_error _ -> [] in
       let nacc = List.rev_append hs acc in
       read_files t nacc


(* With pa_trybind *)
let rec read_files acc names =
  let rec read_one_file lines ch =
    let line = input_line ch with End_of_file -> lines in
    read_one_file (line::lines) ch in
  match names with
  | [] -> List.rev acc
  | h :: t ->
      let ch = open_in h with Sys_error _ -> read_files acc t in
      let fc = read_one_file [] ch in
      let _ = close_in ch in
      let nacc = List.rev_append fc acc in
      read_files nacc t


(* Example 2: 

   val nn: 'a list list -> int -> 'a -> 'a

   Given a list of list $ll$, an integer $n$, a default value $d$, returns the
   element located at index ll[n][n]. If the nth row of ll is absent, print the
   error reason and return d as the result; if the nth row is there but its nth
   column is absent, raise Not_found.
*)


(* Original syntax, through funny exception encoding *)
let nn ll n d =
  try 
    let row = List.nth ll n in
    try List.nth row n 
    with Failure "nth" -> raise Exit
  with 
  | Failure "nth" -> d
  | Exit -> raise Not_found


(* Original syntax, through option types *)
let nn ll n d =
  let row_opt = 
    try Some (List.nth ll n)
    with Failure "nth" -> None in
  match row_opt with
  | Some row -> 
      (try List.nth row n
       with Failure "nth" -> raise Not_found)
  | None -> d


(* with pa_trybind *)
let nn ll n d =
  let row = List.nth ll n with Failure "nth" -> d in
  try List.nth row n with Failure "nth" -> raise Not_found


(* OR *)
let nn ll n d =
  let row = List.nth ll n with Failure "nth" -> d in
  let r = List.nth row n with Failure "nth" -> raise Not_found in 
  r



(* Exampl 3: ufind

   Meaningless connection of "find" function which all raise Not_found, however
   the problem is that you want to handel them seperately. *)

(* in pa_trybind only, you're welcome to traslate  *)
let ufind v lc hc sc =
  let h = List.find lc v with Not_found -> "List" in
  let e = Hashtbl.find hc h with Not_found -> "Hashtbl" in
  let s = Sys.getenv e with Not_found -> "Sys" in
  let i = String.index s sc with Not_found -> "String" in
  string_of_int i;;
