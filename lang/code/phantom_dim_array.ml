

(** several tricks used in this file *)
module DimArray : sig
  type dec  (* = private unit *) 
  type 'a d0 (* =  private unit *) 
  and 'a d1  (* = private unit *) 
  and 'a d2 (* = private unit *) 
  and 'a d3 (* = private unit *) 
  and 'a d4 (* = private unit *) 
  and 'a d5 (* = private unit *) 
  and 'a d6 (* = private unit *) 
  and 'a d7 (* = private unit *) 
  and 'a d8 (* = private unit *) 
  and 'a d9 (* = private unit *) 
  type zero (* = private unit  *) 
  and nonzero (* = private unit*)
  type ('a, 'z) dim0 (* = private int*)
  type 'a dim = ('a, nonzero) dim0
  type ('t, 'd) dim_array = private ('t array)
      
  val dec :            ((dec, zero) dim0 -> 'b) -> 'b
  val d0 : 'a dim        -> ('a d0 dim -> 'b) -> 'b
  val d1 : ('a, 'z) dim0 -> ('a d1 dim -> 'b) -> 'b
  val d2 : ('a, 'z) dim0 -> ('a d2 dim -> 'b) -> 'b
  val d3 : ('a, 'z) dim0 -> ('a d3 dim -> 'b) -> 'b
  val d4 : ('a, 'z) dim0 -> ('a d4 dim -> 'b) -> 'b
  val d5 : ('a, 'z) dim0 -> ('a d5 dim -> 'b) -> 'b
  val d6 : ('a, 'z) dim0 -> ('a d6 dim -> 'b) -> 'b
  val d7 : ('a, 'z) dim0 -> ('a d7 dim -> 'b) -> 'b
  val d8 : ('a, 'z) dim0 -> ('a d8 dim -> 'b) -> 'b
  val d9 : ('a, 'z) dim0 -> ('a d9 dim -> 'b) -> 'b
  val dim : ('a, 'z) dim0 -> ('a, 'z) dim0
  val to_int : ('a, 'z) dim0 -> int
  (* arrays with static dimensions *)

  val make : 'd dim -> 't -> ('t, 'd) dim_array
  val init : 'd dim -> (int -> 'a) -> ('a, 'd) dim_array
  val copy : ('a, 'd) dim_array -> ('a, 'd) dim_array
  (* other array operations go here ... *)
  val get : ('a, 'd) dim_array -> int -> 'a
  val set : ('a, 'd) dim_array -> int -> 'a -> unit
  val combine :
    ('a, 'd) dim_array -> ('b, 'd) dim_array -> ('a -> 'b -> 'c) ->
    ('c, 'd) dim_array
  val length : ('a, 'd) dim_array -> int
  val update : ('a, 'd) dim_array -> int -> 'a -> ('a, 'd) dim_array
  val iter : f:('a -> unit) -> ('a, 'd) dim_array -> unit
  val map : f:('a -> 'b) -> ('a, 'd) dim_array -> ('b, 'd) dim_array
  val iteri : f:(int -> 'a -> unit) -> ('a, 'd) dim_array -> unit
  val mapi : f:(int -> 'a -> 'b) -> ('a, 'd) dim_array ->
    ('b, 'd) dim_array
  val fold_left : f:('a -> 'b -> 'a) -> init:'a -> ('b,'d) dim_array -> 'a
  val fold_right : f:('b -> 'a -> 'a) -> ('b, 'd) dim_array -> init:'a ->
    'a
  val iter2 :
    f:('a -> 'b -> unit) -> ('a,'d) dim_array -> ('b, 'd) dim_array ->
    unit
  val map2 :
    f:('a -> 'b -> 'c) -> ('a, 'd) dim_array -> ('b, 'd) dim_array ->
    ('c, 'd) dim_array
  val iteri2 :
    f:(int -> 'a -> 'b -> unit) -> ('a,'d) dim_array -> ('b, 'd)
    dim_array ->
    unit
  val mapi2 :
    f:(int -> 'a -> 'b -> 'c) -> ('a, 'd) dim_array -> ('b, 'd)
    dim_array ->
    ('c, 'd) dim_array
  val to_array : ('a, 'd) dim_array -> 'a array
end = struct
  include Array
  include Array.Labels
  (** some functions should be overriden later *)  
  type dec = unit
  type 'a d0 = unit
  type 'a d1 = unit
  type 'a d2 = unit
  type 'a d3 = unit
  type 'a d4 = unit
  type 'a d5 = unit
  type 'a d6 = unit
  type 'a d7 = unit
  type 'a d8 = unit
  type 'a d9 = unit
  type zero = unit
  type nonzero = unit

  type ('a, 'z) dim0 = int (* Phantom type *)
  type 'a dim = ('a, nonzero) dim0

  let dec k = k 0

  let d0 d k = k (10 * d + 0)
  let d1 d k = k (10 * d + 1)
  let d2 d k = k (10 * d + 2)
  let d3 d k = k (10 * d + 3)
  let d4 d k = k (10 * d + 4)
  let d5 d k = k (10 * d + 5)
  let d6 d k = k (10 * d + 6)
  let d7 d k = k (10 * d + 7)
  let d8 d k = k (10 * d + 8)
  let d9 d k = k (10 * d + 9)

  let dim d = d

  let to_int d = d

  type ('t, 'd) dim_array = 't array

  let make d x = Array.make (to_int d) x
  let init d f = Array.init (to_int d) f
  let copy x = Array.copy  x
  (* other array operations go here ... *)
  let get : ('a, 'd) dim_array -> int -> 'a = fun a d ->
    Array.get a d

  let set : ('a, 'd) dim_array -> int -> 'a -> unit = fun a d v ->
    Array.set a d v

  let unsafe_get : ('a, 'd) dim_array -> int -> 'a = fun a d ->
    Array.unsafe_get a d

  let unsafe_set : ('a, 'd) dim_array -> int -> 'a -> unit = fun a d v ->
    Array.unsafe_set a d v

  let combine :
      ('a, 'd) dim_array -> ('b, 'd) dim_array -> ('a -> 'b -> 'c) -> ('c,
'd) dim_array =
	fun a b f ->
	  Array.init (Array.length a) (fun i -> f a.(i) b.(i))
            
  let length : ('a, 'd) dim_array -> int = fun a -> Array.length a

  let update : ('a, 'd) dim_array -> int -> 'a -> ('a, 'd) dim_array =
    fun a d v -> let result = Array.copy a in (Array.set result d v;
result)


  let rec iteri2 ~f a1 a2 =
    for i = 0 to length a1 - 1 do
      f i (unsafe_get a1 i) (unsafe_get a2 i)
    done
  let map2 ~f = map2 f
  let mapi2 ~f a1 a2 =
    let l = length a1 in
    if l = 0 then [||] else
    (let r = Array.make l (f 0 (unsafe_get a1 0) (unsafe_get a2 0)) in
     for i = 1 to l - 1 do
       unsafe_set r i (f i (unsafe_get a1 i) (unsafe_get a2 i))
     done;
     r)

  let to_array : ('a, 'd) dim_array -> 'a array = fun d -> d

end;;

open DimArray
let d10 = dec d1   d0 dim
let a = make d10 0.
let b = make d10 1.


module S : sig
  type 'a t = private ('a array)
  val of_float_array : 'a array -> 'a t
end = struct
  type 'a t = 'a array
  let of_float_array = fun x -> x 
end 
