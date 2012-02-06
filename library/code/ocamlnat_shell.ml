open Shell

(**
   command :
   ?cmdname:string          ->
   ?arguments:string array  ->
   ?chdir:string            ->
   ?environment:Shell_sys.environment ->

   ?descriptors:Unix.file_descr list ->
   (*
     The list of file descriptors to share with the current process
   *)
   ?assignments:assignment list -> string -> Shell_sys.command
   (*
     The list of descriptors assignments
   *)
   All assignments

   cmd: arguments should be specified explicit

   call:
   ?ignore_error_code:bool ->
   ?mode:Shell_sys.group_mode ->
   ?stdin:producer ->
   ?stdout:consumer -> 
   ?stderr:consumer -> Shell_sys.command list -> unit

   stdin,stdout,stderr : redirection

   (>&) : file_descr -> file_descr -> assignment
   from_string: ?pos:int ->?len:int -> ?epipe:(unit -> unit) -> string -> producer
   from_string: prducer
   from_(function, file, fd, dev_null)

   to_(buffer,function)

*)

let _ = begin 
  call [command ~chdir:".." "ls"];
end 

let res = begin
  let bf = Buffer.create 100 in 
  call ~stdout:(to_buffer bf)[command "ls" ~arguments:[|"-al"|]];
  Buffer.contents bf |> Pervasives.(output_string stdout);
end 

(**
  subprocess errors are caught and propagated to the caller
*)

let _ = begin
  call [cmd "ocamlfind " ["query"; "shell"]; cmd "ls"];
end 
(** #u "cat `ocamlfind query shell`/META";;     *)

let _ = begin
  let b = Buffer.create 100 in 
  try call ~stdout:(to_buffer b)
    [command
    ~assignments:[stderr >& stdout]
    ~arguments:[|"/a"|]
    "ls"];
  with _ -> Buffer.contents b |> Pervasives.(output_string stdout);
end 

(** error reporting is better than a traditional shell, because the
    exit codes of all started commands are returned.
*)
  
