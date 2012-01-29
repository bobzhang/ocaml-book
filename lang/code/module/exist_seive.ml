
module type Stream = sig
  type state
  val start : state
  val next : state -> state
  val value : state -> int
end

module Two : Stream = struct
  type state = int
  let start = 2
  let next = fun (i:int) -> succ i
  let value = fun (i:int) -> i 
end

module type State = Stream
module Start = (Two : State)  


module Next (S:State) : State = struct
  type state = S.state
  let rec filter : state -> state =
    fun (s:state) ->
      if (S.value s) mod (S.value S.start) = 0
      then filter (S.next s)
      else s
  let start = filter S.start
  let next =  fun (s:state) -> filter (S.next s)
  let value = S.value
end 

module Sieve : Stream = struct
  type state = (module Stream)
  let start = (module Two : Stream)
  let next = fun (s:state) ->
    let module S = (val s : Stream) in
    (module Next(S) : Stream)
  let value = fun (s:state)->
    let module S = (val s : Stream) in
    S.value S.start
end 

let rec nthstate : int -> Sieve.state = 
  fun (n:int) ->
    if n = 0
    then Sieve.start
    else Sieve.next (nthstate (pred n))

  (* The nth prime (where 2 is the zeroth) *)
let nthprime = fun (n:int) -> Sieve.value (nthstate n)
  
