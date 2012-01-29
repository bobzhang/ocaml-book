
module Russo =
struct
  (* An abstract type of integer streams *)
  module type Stream =
  sig
    type state
    val start : state
    val next  : state -> state
    val value : state -> int
  end

  (* An instance of the stream abstract type: the natural numbers starting from 2 *)
  module TwoOnwards : Stream =
  struct
    type state = int
    let start = 2
    let next = fun (i:int) -> succ i
    let value = fun (i:int) -> i
  end

  (* In this implementation of the Sieve of Eratosthenes, the state is
     a stream and the initial state is the stream of natural numbers
     starting from 2 *)
  module type State = Stream
  module Start = (TwoOnwards : State)

  (* One step of the algorithm. *)
  module Next (S:State) : State =
  struct
    type state = S.state
    let rec filter : state -> state =
      fun (s:state) ->
        if (S.value s) mod (S.value S.start) = 0
        then filter (S.next s)
        else s
    let start = filter S.start
    let next = fun (s:state) -> filter (S.next s)
    let value = S.value
  end
    
  (* The sieve, a stream of streams:

        TwoOnwards, Next(TwoOnwards), Next(Next(TwoOnwards)), ...
  *)
  module Sieve : Stream =
  struct
    type state = (module Stream)
    let start = (module TwoOnwards : Stream)
    let next = fun (s:state) ->
                 let module S = (val s : Stream) in
                   (module Next(S) : Stream)
    let value = fun (s:state) ->
                  let module S = (val s : Stream) in
                    S.value S.start
  end
    
  (* The nth state of the sieve algorithm *)
  let rec nthstate : int -> Sieve.state = 
    fun (n:int) ->
      if n = 0
      then Sieve.start
      else Sieve.next (nthstate (pred n))

  (* The nth prime (where 2 is the zeroth) *)
  let nthprime = fun (n:int) -> Sieve.value (nthstate n)
end


(* This is a mechanical translation of Russo's stream example into
   OCaml using polymorphic record fields and the well-known encoding
   of existentials using universals.  The benefits of first-class
   modules over polymorphism are relatively limited here: they're
   neater notationally, but not really any more powerful for this
   example.
*)
module Russo_polymorphism =
struct
  (* An abstract type of integer streams *)

  type 'state stream_ops = {
    start : 'state;
    next  : 'state -> 'state;
    value : 'state -> int;
  }
  type 'ctxt stream_cont = {
    streamk : 'state. 'state stream_ops -> 'ctxt
  }
  type stream  = {
    stream : 'ctxt. 'ctxt stream_cont -> 'ctxt 
  }

  (* An instance of the stream abstract type: the natural numbers starting from 2 *)
  let two_onwards : stream = {
    stream = fun k -> k.streamk {
      start = 2;
      next = (fun (i:int) -> succ i);
      value = (fun (i:int) -> i);
    }
  }

  (* In our implementation of the Sieve of Eratosthenes, the state is a
     stream and the initial state is the stream of natural numbers
     starting from 2 *)
  type state = stream
  let start : state = two_onwards

  (* One step of the algorithm. *)
  let next ss =
    ss.stream {
      streamk = 
        fun { start = s_start; next = s_next; value = s_value; } ->
          let rec filter =
            fun s ->
              if (s_value s) mod (s_value s_start) = 0
              then filter (s_next s)
              else s in
            {
              stream = fun k -> k.streamk {
                start = filter s_start;
                next = (fun s -> filter (s_next s));
                value = s_value;
              }
            }
    }

  (* The sieve:

        two_onwards, next(two_onwards), next(next(two_onwards)), ...
  *)
  module Sieve =
  struct
    type state = stream
    let start = (two_onwards : stream)
    let next = fun (s:state) -> next s
    let value = fun (s:state) -> s.stream { streamk = fun { start; value } -> value start }
  end
    
  (* The nth state of the sieve algorithm *)
  let rec nthstate : int -> Sieve.state = 
    fun (n:int) ->
      if n = 0
      then Sieve.start
      else Sieve.next (nthstate (pred n))

  (* The nth prime (where 2 is the zeroth) *)
  let nthprime = fun (n:int) -> Sieve.value (nthstate n)
end


(* First class modules also make it possible to encode higher-kinded
   existential types, which can *not* be encoded using polymorphic
   records in OCaml.  In particular, it doesn't seems to be possible
   to express the `map' function of the following stream type using
   the encoding based on polymorphic records. *)
module Parameterised_existentials =
struct
  (* The operations of the stream interface *) 
  (*
    ∃(t :: ∗ -> ∗){
      head : ∀α. α t → α,
      tail : ∀α. α t → α t,
      from : ∀α. (int → α) → α t,
      map  : ∀α β. (α → β) → α t → β t
    }
  *)
  module type StreamOps =
  sig
    type 'a t
    val head :  'b. 'b list -> 'b
    val tail : 'a. 'a t -> 'a t
    val from : 'a. '(int -> 'a) -> 'a t
    val map  : 'a 'b. ('a -> 'b) -> 'a t -> 'b t
  end

  (* The type of a stream value.  We'll expose the type of elements
     ('a'), but leave the type of streams ('t') abstract. *)
  module type Stream =
  sig
    type a
    include StreamOps
    val stream : a t
  end

  (* A convenient shorthand for the type of stream values *)
  type 'a stream = (module Stream with type a = 'a)

  (* An implementation of streams as cons cells with lazy tails *)
  module LazyStream : StreamOps =
  struct
    type 'a t = Cons of 'a * 'a t Lazy.t
    let head (Cons (h, _)) = h
    let tail (Cons (_, lazy t)) = t
    let from f =
      let rec mk n = Cons (f n, lazy (mk (n + 1))) in
        mk 0
    let rec map f (Cons (h, lazy t)) = Cons (f h, lazy (map f t))
  end

  (* An implementation of streams as functions *)
  module FunctionalStream : StreamOps =
  struct
    type 'a t = int -> 'a
    let head s = s 0
    let tail s = fun n -> s (n + 1)
    let from f = f
    let map f s = fun n -> f (s n)
  end

  (* Create a stream from an implementation of the interface and an indexing function *)
  let mk_stream : 'a.(module StreamOps) -> (int -> 'a) -> 'a stream
    = fun (type s) ops f ->
      (module 
       struct
         type a = s
         include (val ops : StreamOps)
         let stream = from f
       end : Stream with type a = s)

  (* Expose the map function as a standalone value. *)
  let map : 'a. ('a -> 'b) -> 'a stream -> 'b stream =
    fun (type s) (type t) f stream -> 
      let module Stream = (val stream : Stream with type a = s) in
        (module
         struct
           type a = t
           include (Stream : StreamOps with type 'a t = 'a Stream.t)
           let stream = Stream.map f Stream.stream
         end : Stream with type a = t)
end
