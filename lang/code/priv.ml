

module Int = struct
  type t = int
  let of_int x = x
  let to_int  x = x 
end

module Priv : sig
  type t = private int
  val of_int : int -> t
  val to_int : t -> int
end = Int

  
module Abstr : sig
  type t 
  val of_int : int -> t
  val to_int : t -> int
end = Int

let _ =
  print_int (Priv.of_int 3 :> int)

let _ =
  List.iter (print_int|-print_newline)
    ([Priv.of_int 1; Priv.of_int 3] :> int list)

(** non-container type *)    
type 'a f =
  |A of (int -> 'a)
  |B

(** this is is hard to do when abstract types *)      
let  a =
  ((A (fun x -> Priv.of_int x )) :> int f) 
