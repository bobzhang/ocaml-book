open Sys


(** ignore the sighup signal *)  
let _ = 
  signal sighup Signal_ignore

let _ =
  Unix.execvp argv.(1)
    (Array.sub argv 1 (Array.length argv - 1))
