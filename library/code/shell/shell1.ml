(* #require "shell";; *)

open Shell


let s = "d\na\nc\nb\n"
let b = Buffer.create 20

let _ =
  call ~stdin:(from_string s) ~stdout:(to_buffer b) [cmd "sort" [] ]

let _ =
  call [cmd "cat" ["/notfound"]; cmd "ls" ["/notfound.too"] ]

(** if the file "fail" refers to a non-existing interpreter #!
    /not/found but is executable, this special error can only be
    detected by the "exec" call. Unix shells print an error message to
    stderr, and return an exit code of 127 (which is reserved for this
    case): Sys.command "fail";; sh: ./fail: No such file or directory
    ~ : int = 127

    However, the true reason isn't reported. In contrast to this, the
    Shell module is able to pass the real error condition back to the
    calling program:
*)

let _ = call [command "fail"]    
(** Exception: Shell_sys.Executable_not_found "fail". *)

