open Format

(* unpacking *)  
let sort (type s) set l =
  let module Set = (val set : Set.S with type elt = s) in
  Set.elements (List.fold_right Set.add l Set.empty)

(* packing*)    
let make_set (type s) cmp =
  let module S = Set.Make(struct
    type t = s
    let compare = cmp
   end) in
  (module S:Set.S with type elt = s)

(*    
module type S = sig
  type t=int
  module  X : sig
    type 'a u
    val add : 'a u -> 'a u -> int       
  end
end

let f (type a) ( module Y : S with type int X.u  = a)  (y: int Y.X.u) =
  Y.X.add y y 

*)

module type S0 = sig
  type t
  val add : t -> t -> int 
end
(* f : (module S0 with type t = a ) -> a -> int *)      
let f (type a) (module Y : S0 with type t = a) (y:Y.t)  =
  Y.add y y

(*
module type S1 = sig
  module Inner  : sig
    type 'a t
    val add : 'a t -> 'a t -> int 
  end
  type int_u 
end 

let f1
    (type s)
    (type a)
    (module Y : S1  with type int_u = int Y.Inner.t ) (y: Y.int_u) = 
  Y.Inner.add y y 
*)

(*
module type FOO = sig
  type t 
end 

module type BAR = sig
  type foo_t 
  module Foo: FOO with type t = foo_t
end 


let f (type a) (module M: BAR with type foo_t = a) =
  ()
*)

type ('a,'t) set =
    (module Set.S with type elt = 'a and type t = 't)

let add2 (type a) (type t)
    (m : (a,t)set) x y s =
  let module S = (val m) in 
  S.add x (S.add y s)

module type MapT = sig
  include Map.S
  type data
  type map
  val of_t : data t -> map
  val to_t : map -> data t
end 
type ('k,'d,'m) map =
    (module MapT with type key = 'k
    and type data ='d and type map = 'm)




let add (type k) (type d) (type m)
    (m:(k,d,m) map) x y s =
  let module M = (val m) in
  M.(of_t (add x y (to_t s)))

module SSMap = struct
  include Map.Make(String)
  type data = string
  type map = data t
  let of_t x = x
  let to_t x = x     
end 

let (ssmap : (string,string, SSMap.map) map) = (module SSMap :
            MapT with type key = string
            and type data = string
            and type map = SSMap.map)

      









      






