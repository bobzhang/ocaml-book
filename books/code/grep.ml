
open Sys
open Unix

let grep () =
  execvp "grep" (Array.concat
                   [ [|"grep"; "-i"|];
                     (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))])
    


let emacs () =
  execve "/usr/bin/emacs" Sys.argv
    (Array.concat
       [ [|"TERM=hacked-xterm"|]; (environment ())])
    
let _ =
  handle_unix_error grep ()

let _ =
  handle_unix_error emacs ()
