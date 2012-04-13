


let p (f:int)  = print_string <:here<
  ${f,%ld} 
>>

(**
   let p (f : int) =
  print_string (String.concat "" [ "
  "; Printf.sprintf "%ld" f; " 
" ])

*)  
