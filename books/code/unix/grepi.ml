open BatPervasives

let grep () = Unix.(
  execvp "grep"
    (Array.append
       [|"grep"; "-i"|]
       (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
    )
    
)

let _ =
  Unix.handle_unix_error grep ()
  




















