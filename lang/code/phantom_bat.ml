
module C = String.Cap


(** closed type here when you want it to be read only *)  
let read : [ `Read] C.t = C.of_string "aghsogho"

(**
   error
   let _ = C.set s 0 'a'  ; s
*)

let a =
  C.set read 0 'b' ;
  C.get read 0


(** open for write and read *)    
let write : [> `Write] C.t    = C.of_string "aghso"

let _ =
  C.set write 0 'a';
  print_char (C.get write 0);
  C.to_string write

(** now
write;;
- : [ `Read | `Write ] C.t = <abstr>
*)  
