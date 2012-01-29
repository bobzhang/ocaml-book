
module type StreamOps = sig
  type 'a t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val from : (int -> 'a ) -> 'a t
  val map : ('a -> 'b ) -> 'a t -> 'b t 
end 
(* First class modules also make it possible to encode higher-kinded
   existential types, which can *not* be encoded using polymorphic
   records in OCaml.  In particular, it doesn't seems to be possible
   to express the `map' function of the following stream type using
   the encoding based on polymorphic records.
   The operations of the stream interface 
  
    ∃(t :: ∗ -> ∗){
   head : ∀α. α t → α,
   tail : ∀α. α t → α t,
   from : ∀α. (int → α) → α t,
   map  : ∀α β. (α → β) → α t → β t
   }
   
*)
module type Stream = sig
  type a
  include StreamOps
  val stream : a t
  (** t is actually of kind * -> *
      you can not write
      module type S = sig type a  type a t  end
      directly
  *)
end 

module LStream: StreamOps = struct
  type 'a t = Cons of 'a * 'a t Lazy.t
  let head (Cons (h,_)) = h
  let tail (Cons (_, lazy t )) =  t
  let from f =
    let rec mk n = Cons (f n , lazy (mk (n+1))) in
    mk 0
  let rec map f (Cons (h, lazy t)) =
    Cons (f h, lazy (map f t))
end 

module FStream: StreamOps = struct
  type 'a t = int -> 'a
  let head s = s 0
  let from f = f
  let map f s = fun n -> f (s n)
end
  
type 'a stream = (module Stream with type a = 'a)
    
let mk_stream : 'a. (module StreamOps) -> (int -> 'a ) -> 'a stream =
  fun (type s ) ops f ->
    (module struct
      type a = s
      include (val ops: StreamOps)
      let stream = from f 
    end : Stream with type a = s )

let map : 'a. ('a -> 'b) -> 'a stream -> 'b stream = fun (types s ) (type t)
  f stream ->
    let module Stream = (val stream: Stream with type a = s) in
    (module struct
      type a = t
      include (Stream : StreamOps with type 'a t = 'a Stream.t)
      let stream = Stream.map f Stream.stream
    end : Stream with type a = t)
