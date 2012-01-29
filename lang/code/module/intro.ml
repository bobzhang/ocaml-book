module type ID = sig val id : 'a -> 'a end

let f m = 
    let module Id = (val m : ID) in 
    (Id.id 1, Id.id true);;

(* val f : (module ID) -> int * bool = <fun>   *)

f (module struct let id x = print_endline "ID!"; x end : ID);;
(*
  ID!
  ID!
*)  
