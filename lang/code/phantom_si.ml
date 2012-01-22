module Length : sig
  type 'a t = private float 
  val meters : float -> [`Meters] t
  val feet : float -> [`Feet] t
  val (+.) : 'a t -> 'a t -> 'a t
  val to_float : 'a t -> float
end = struct
  type 'a t = float
  external meters : float -> [`Meters] t = "%identity"
  external feet : float -> [`Feet] t = "%identity"
  let (+.) = (+.)
  external to_float : 'a t -> float = "%identity"
end




let meters, feet = Length.(meters,feet)
let m1 = meters 10. 
let m2 = meters 20.
open Printf  

let _ =
  printf "10m + 20m = %g\n" Length.(( (m1 +. m2 ) :> float ))

let f1 = feet 40. 
let f2 = feet 50. 

let _ =   printf "40ft + 50ft = %g\n" (Length.( ((f1 +. f2) :> float )))
  
  (*printf "10m + 50ft = %g\n" (to_float (m1 +. f2)) (* error *) *)


module Connection : sig
  type 'a t

  (** we return a closed Readonly type *)
  val connect_readonly : unit -> [`Readonly] t

  (** we reeturn  a closed ReadWrite both type *)
  val connect : unit -> [`Readonly|`Readwrite] t

  (** read only or greater *)
  val status : [>`Readonly] t -> int
    
  val destroy : [>`Readwrite] t -> unit
end = struct
  type 'a t = int
  let count = ref 0
  let connect_readonly () = incr count; !count
  let connect () = incr count; !count
  let status c = c
  let destroy c = ()
end  
