(* The Caml code for the various monads of lecture 4 *)

module type MONAD = sig
  type 'a mon
  val ret: 'a -> 'a mon
  val bind: 'a mon -> ('a -> 'b mon) -> 'b mon
  type 'a result
  val run: 'a mon -> 'a result
end

(* The Exception monad, a.k.a. the Error monad *)

module Exception = struct

  type 'a mon = V of 'a | E of exn

  let ret (x: 'a) : 'a mon = V x
  let bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    match m with E exn -> E exn | V x -> f x

  type 'a result = 'a 
  let run (m: 'a mon) : 'a result =
    match m with V x -> x | E exn -> failwith "uncaught exception"

  let raise (x: exn) : 'a mon = E x
  let trywith (m: 'a mon) (f: exn -> 'a mon): 'a mon =
    match m with E exn -> f exn | V x -> V x
end

(* The State monad, specialized to references over integers *)

module Store = struct
  module IntMap = Map.Make(struct type t = int let compare = compare end)
  type t = { next: int; contents: int IntMap.t }
  type ref = int
  let empty = { next = 0; contents = IntMap.empty }
  let alloc x s =
      (s.next,
       { next = s.next + 1; contents = IntMap.add s.next x s.contents })
  let read r s = IntMap.find r s.contents
  let write r x s =
     { next = s.next; contents = IntMap.add r x s.contents }
end

module State = struct
  type 'a mon = Store.t -> 'a * Store.t

  let ret (x: 'a) : 'a mon = fun v -> (x, v)
  let bind (x: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun v -> let (x', v') = x v in f x' v'

  type 'a result = 'a 
  let run (c: 'a mon) : 'a result = fst (c Store.empty)

  let ref (n : int) : Store.ref mon =
    fun s -> Store.alloc n s
  let deref (r: Store.ref) : int mon =
    fun s -> (Store.read r s, s)
  let assign (r: Store.ref) (x: int) : unit mon =
    fun s -> ((), Store.write r x s)
end

let sumlist l =
  State.run (State.bind (State.ref 0) (fun r ->
    let rec sum = function
    | [] -> State.deref r
    | hd :: tl -> State.bind (State.deref r) (fun n ->
                  State.bind (State.assign r (n + hd)) (fun _ ->
                  sum tl))
    in sum l))

(* The Continuation monad, assuming the type of answers is "int" *)

module Cont = struct

  type answer = int
  type 'a mon = ('a -> answer) -> answer

  let ret (x: 'a) : 'a mon = fun k -> k x
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun k -> m (fun x -> f x k)

  let run (m: answer mon) = m (fun x -> x)

  let callcc (f: ('a -> answer) -> 'a mon) : 'a mon =
    fun k -> f k k
  let throw (k: 'a -> answer) (x: 'a) : 'a mon =
    fun _ -> k x
end

let exple_cont n =
  Cont.run 
    (Cont.callcc (fun k ->
       Cont.bind
         (if n < 0 then Cont.throw k n else Cont.ret n)
         (fun x -> Cont.ret (x + 1))))

(* The Counting monad *)

module Count = struct
    type 'a mon = int -> 'a * int

    let ret (x: 'a) : 'a mon = fun n -> (x, n)
    let bind (m: 'a mon) (f: 'a -> 'b mon) =
      fun n -> match m n with (x, n') -> f x n'

    let run (m: 'a mon) : 'a * int = m 0

    let tick (m: 'a mon) : 'a mon = fun n -> m (n+1)
end

let rec insert x l =
  match l with
  | [] -> Count.ret [x]
  | h :: t ->
     Count.bind (Count.tick (Count.ret (x < h)))
       (fun b -> if b 
                 then Count.ret (x::l)
                 else Count.bind (insert x t)
                                 (fun r -> Count.ret(h::r)))

let test_count =
  Count.run (insert 3 [1;2;3;4;5;6])

(* The Logging monad, a.k.a the Writer monad *)

module Log = struct
  type log = string list
  type 'a mon = log -> 'a * log

  let ret (x: 'a) : 'a mon = fun l -> (x, l)
  let bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun l -> match m l with (x, l') -> f x l'

  type 'a result = 'a * log
  let run (m: 'a mon): 'a result =
    match m [] with (x, l) -> (x, List.rev l)

  let log msg : unit mon = fun l -> ((), msg :: l)
end

let log_abs n =
  Log.run
    (if n >= 0 
     then Log.bind (Log.log "positive") (fun _ -> Log.ret n)
     else Log.bind (Log.log "negative") (fun _ -> Log.ret (-n)))

(* Non-determinism, a.k.a. the List monad *)

module Nondet = struct

  type 'a mon = 'a list

  let ret (x: 'a): 'a mon = [x]
  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    match m with [] -> [] | hd :: tl -> f hd @ bind tl f

  type 'a result = 'a list
  let run (m: 'a mon): 'a result = m

  let fail : 'a mon = []
  let either (a: 'a mon) (b: 'a mon): 'a mon = a @ b
end

let rec insert x l =
  Nondet.either (Nondet.ret (x :: l))
    (match l with
     | [] -> Nondet.fail
     | hd :: tl ->
         Nondet.bind (insert x tl)
                     (fun l' -> Nondet.ret (hd :: l')))

let rec permut l =
  match l with
  | [] -> Nondet.ret []
  | hd :: tl ->
      Nondet.bind (permut tl) (fun l' -> insert hd l')

let test_nondet =
  Nondet.run (permut [1;2;3])

(* The Parsing monad, a.k.a. the Reader monad  *)

module Parsing = struct

  type 'a result =
    | Success of 'a * char list
    | Failure

  type 'a mon = char list -> 'a result

  let ret (x: 'a): 'a mon = fun txt -> Success(x, txt)

  let bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun txt ->
      match m txt with
      | Failure -> Failure
      | Success(x, txt') -> f x txt'

  let either (m1: 'a mon) (m2: 'a mon): 'a mon =
    fun txt ->
      match m1 txt with
      | Failure -> m2 txt
      | Success(x, txt') as res -> res

  let symbol (c: char): char mon =
    fun txt ->
      match txt with
      | [] -> Failure
      | c' :: txt' -> if c = c' then Success(c, txt') else Failure
  
  let symbolclass (pred: char -> bool): char mon =
    fun txt ->
      match txt with
      | [] -> Failure
      | c :: txt' -> if pred c then Success(c, txt') else Failure
  
  (* Some derived operations *)

  let opt (m: 'a mon): 'a option mon =
    either (bind m (fun x -> ret (Some x))) (ret None)

  let rec star (m: 'a mon): 'a list mon =
    either (plus m) (ret [])

  and plus (m: 'a mon): 'a list mon =
    bind m (fun x -> bind (star m) (fun y -> ret (x::y)))

end

(* Randomized computations *)

module type RANDOM = sig
  type 'a mon
  val ret: 'a -> 'a mon
  val bind: 'a mon -> ('a -> 'b mon) -> 'b mon
  val choose: float -> 'a mon -> 'a mon -> 'a mon
  val rand: int -> int mon
end

module Random_examples(M: RANDOM) = struct

  let roll_3d6 =
    M.bind (M.rand 6) (fun d1 ->
    M.bind (M.rand 6) (fun d2 ->
    M.bind (M.rand 6) (fun d3 -> 
        M.ret (1+d1 + 1+d2 + 1+d3))))

  type light = Green | Yellow | Red

  let traffic_light =
    M.choose 0.1 (M.ret Yellow)
                 (M.choose 0.5 (M.ret Red) 
                               (M.ret Green))
end

(* The Simulation monad *)

module Simulation = struct
  type state = int
  type 'a mon = state -> 'a * state

  let ret (x: 'a) : 'a mon = fun s -> (x, s)
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun s -> let (x', s') = m s in f x' s'
  let run (seed: state) (m: 'a mon) = fst (m seed)

  let next_state (s: state) : state = s * 25173 + 1725

  let rand (n: int) : int mon =
    fun s -> ((abs s) mod n, next_state s)

  let choose (p: float) (a: 'a mon) (b: 'a mon) : 'a mon =
    fun s ->
      if float (abs s) <= p *. float max_int
      then a (next_state s)
      else b (next_state s)
end

module S = Random_examples(Simulation)

let exple1 = Simulation.run 123456 S.roll_3d6
let exple2 = Simulation.run 7890 S.roll_3d6
let exple3 = Simulation.run 4567 S.traffic_light

(* The Distribution monad *)

module Distribution = struct

  type 'a mon = ('a * float) list (* sum of floats = 1.0 *)

  let ret (x: 'a) : 'a mon = [x, 1.0]
  let mulp p l = List.map (fun (x, p') -> (x, p *. p')) l
  let rec bind (x: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    match x with [] -> [] | (hd, p) :: tl -> mulp p (f hd) @ bind tl f

  let choose (p: float) (x: 'a mon) (y: 'a mon) : 'a mon =
    mulp p x @ mulp (1.0 -. p) y
  let rand (n: int) : int mon =
    let p = 1.0 /. float n in
    let rec rnd n = if n < 0 then [] else (n, p) :: rnd (n-1) in
    rnd (n-1)

  let rec accumulate x = function
    | [] -> 0.0
    | (hd, p) :: tl -> if x = hd then p +. accumulate x tl else accumulate x tl

  let rec filter x = function
    | [] -> []
    | (hd, p as p_hd) :: tl -> if x = hd then filter x tl else p_hd :: filter x tl

  let rec compact (x: 'a mon) : 'a mon =
    match x with
    | [] -> x
    | [_] -> x
    | (hd, p) :: tl -> (hd, accumulate hd x) :: compact (filter hd tl)

  let run (m: 'a mon) = compact m
end

module D = Random_examples(Distribution)

let exple5 = Distribution.run D.roll_3d6
let exple6 = Distribution.run D.traffic_light


(* The Expectation monad *)

module Expectation = struct

  type 'a mon = ('a -> float) -> float

  let ret (x: 'a) : 'a mon = fun k -> k x
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon =
    fun k -> m (fun x -> f x k)

  let choose (p: float) (x: 'a mon) (y: 'a mon) : 'a mon =
    fun k -> p *. x k +. (1.0 -. p) *. y k

  let rand (n: int) : int mon =
    fun k -> 
      let rec sum n = if n <= 0 then 0.0 else k (n-1) +. sum (n-1) in
      sum n /. float n

  (* Return the expectation of a given result value *)
  let run (res: 'a) (m: 'a mon) =
    m (fun x -> if x = res then 1.0 else 0.0)
end

module E = Random_examples(Expectation)

let exple8 = Expectation.run 16 E.roll_3d6
let exple9 = Expectation.run E.Green E.traffic_light


(**** Monad transformers *)

(* The Identity monad *)

module Identity = struct
  type 'a mon = 'a
  let ret x = x
  let bind m f = f m
  type 'a result = 'a
  let run m = m
end

(* The monad transformer for exceptions *)

module ExceptionTransf(M: MONAD) = struct
  type 'a outcome = V of 'a | E of exn
  type 'a mon = ('a outcome) M.mon

  let ret x = M.ret (V x)
  let bind m f = 
    M.bind m (function E e -> M.ret (E e) | V v -> f v)
  let lift x = M.bind x (fun v -> M.ret (V v))

  type 'a result = 'a M.result

  let run m = M.run (M.bind m (function V x -> M.ret x))

  let raise e = M.ret (E e)
  let trywith m f =
    M.bind m (function E e -> f e | V v -> M.ret (V v))
end

(* The monad transformer for state *)

module StateTransf(M: MONAD) = struct

  type 'a mon = Store.t -> ('a * Store.t) M.mon

  let ret x = fun s -> M.ret (x, s)
  let bind m f =
    fun s -> M.bind (m s) (fun (x, s') -> f x s')
  let lift m = fun s -> M.bind m (fun x -> M.ret (x, s))

  type 'a result = 'a M.result

  let run m =
    M.run (M.bind (m Store.empty) (fun (x, s') -> M.ret x))

  let ref x = fun s -> M.ret (Store.alloc x s)
  let deref r = fun s -> M.ret (Store.read r s, s)
  let assign r x = fun s -> M.ret (Store.write r x s)
end

(* The monad transformer for continuations *)

module ContTransf(M: MONAD) = struct
  type answer = int
  type 'a mon = ('a -> answer M.mon) -> answer M.mon

  let ret (x: 'a) : 'a mon = fun k -> k x
  let bind (m: 'a mon) (f: 'a -> 'b mon) : 'b mon = fun k -> m (fun v -> f v k)
  let lift (m: 'a M.mon): 'a mon = fun k -> M.bind m k
  let run (m: 'a mon) : answer M.result = M.run (m (fun x -> M.ret x))

  let callcc f = fun k -> f k k
  let throw c x = fun k -> c x
end

(* Example of combinations *)

module StateAndException = struct
  include ExceptionTransf(State)
  let ref x = lift (State.ref x)
  let deref r = lift (State.deref r)
  let assign r x = lift (State.assign r x)
end

module ContinuationAndState = struct
  include ContTransf(State)
  let ref x = lift (State.ref x)
  let deref r = lift (State.deref r)
  let assign r x = lift (State.assign r x)
end
  
(** The Concurrency monad transformer, in the style of Tomas Petricek
  ("Fun with Parallel Monad Comprehensions", The Monad.Reader, vol 18, 
   July 2011) *)

module Concur(M: MONAD) = struct

  type 'a mon =
    | Done of 'a
    | Step of ('a mon) M.mon

  let rec perform (x: 'a mon): 'a M.mon =
    match x with
    | Done res -> M.ret res
    | Step m -> M.bind m perform

  let act (m: 'a M.mon): 'a mon =
    Step (M.bind m (fun res -> M.ret (Done res)))

  let ret (x: 'a): 'a mon = Done x

  let rec bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    match m with
    | Done res -> f res
    | Step s -> Step (M.bind s (fun m' -> M.ret (bind m' f)))

  type 'a result = 'a M.result

  let run (x: 'a mon): 'a result =
    M.run (perform x)

  let rec par (m1: 'a mon) (m2: 'b mon) : ('a * 'b) mon =
    match m1, m2 with
    | Done r1, Done r2 -> Done (r1, r2)
    | Step s1, Step s2 ->
        Step (M.bind s1 (fun m1' ->
              M.bind s2 (fun m2' -> M.ret (par m1' m2'))))
    | Done r1, Step s2 ->
        Step (M.bind s2 (fun m2' -> M.ret (par (Done r1) m2')))
    | Step s1, Done r2 ->
        Step (M.bind s1 (fun m1' -> M.ret (par m1' (Done r2))))

end

module M = Concur(Log)

let rec loop n s =
    if n <= 0
    then M.ret ()
    else M.bind (M.act (Log.log s)) (fun _ -> loop (n-1) s)

let exple_concur =
  M.run (M.bind (M.act (Log.log "start:")) (fun _ ->
                 M.par (loop 6 "a") (loop 4 "b")))

(* Another Concurrency monad transformer, in the style of Koen Claessen
   ("A poor man's concurrency monad", J. Func. Prog 9(3), 1999).  *)

module Concur2(M: MONAD) = struct
  type answer = 
    | Seq of answer M.mon
    | Par of answer * answer
    | Stop
  type 'a mon = ('a -> answer) -> answer

  let ret (x: 'a): 'a mon = fun k -> k x
  let bind (m: 'a mon) (f: 'a -> 'b mon): 'b mon =
    fun k -> m (fun v -> f v k)

  let atom (m: 'a M.mon): 'a mon =
    fun k -> Seq(M.bind m (fun v -> M.ret (k v)))
  let stop : 'a mon = fun k -> Stop
  let par (m1: 'a mon) (m2: 'a mon): 'a mon = fun k -> Par (m1 k, m2 k)

  let rec schedule acts =
    match acts with
    | [] -> M.ret ()
    | Seq m :: rem ->
          M.bind m (fun m' -> schedule (rem @ [m']))
    | Par(a1, a2) :: rem ->
          schedule (a1 :: a2 :: rem)
    | Stop :: rem ->
          schedule rem
  type 'a result = 'a M.result
  let run (m: 'a mon) : 'a result = M.run (schedule [m (fun _ -> Stop)])

end

module M2 = Concur2(Log)

let rec loop n s =
    if n <= 0
    then M2.ret ()
    else M2.bind (M2.atom (Log.log s)) (fun _ -> loop (n-1) s)

let exple_concur =
  M2.run (M2.bind (M2.atom (Log.log "start:")) (fun _ ->
          M2.par (loop 6 "a") (loop 4 "b")))

(* The comonad of lazy evaluation. *)

module CoLazy = struct

    type 'a com = 'a status ref
    and 'a status = 
      | Evaluated of 'a
      | Suspended of (unit -> 'a)

    let proj (x: 'a com): 'a =
      match !x with
      | Evaluated v -> v
      | Suspended f -> let v = f() in x := Evaluated v; v

    let cobind (f: 'a com -> 'b) (x: 'a com) : 'b com =
      ref (Suspended (fun () -> f x))

    let init : unit com = ref (Evaluated ())

end

(* A comonad for 1d cellular automata.
   Taken from Dan Piponi, "Evaluating cellular automata is comonadic", 2006. *)

module Cells = struct

  type 'a stream = 'a cell Lazy.t
  and 'a cell = Cell of 'a * 'a stream

  (* The state of a 1d cellular automata is a bidirectional infinite sequence
        ... x(-3) x(-2) x(-1) x(0) x(1) x(2) x(3) ...
     which we represent as a triple
        ( x(-1) x(-2) x(-3) ....,     (* a lazy stream *)
          x(0),
          x(1) x(2) x(3) ....)        (* another lazy stream *)

  type 'a com = 'a stream * 'a * 'a stream

  (* Project the state at origin *)

  let proj ((l,x,r): 'a com): 'a = x

  (* Shift the state left or right one cell *)

  let left ((lazy(Cell(l1,ll)), x, r): 'a com): 'a com =
    (ll, l1, lazy(Cell(x, r)))

  let right ((l, x, lazy(Cell(r1,rr))): 'a com): 'a com =
    (lazy(Cell(x, l)), r1, rr)

  (* "cobind f c" applies f to all possible shifts of c, returning
        ... f(left^n c) ... f(left(left c)) f(left c)
        f(c)
        f(right c) f(right(right c)) ... f(right^n(c)) ... *)

  let rec map_iterate f shift c =
    lazy(Cell(f c, map_iterate f shift (shift c)))

  let cobind (f: 'a com -> 'b) (c: 'a com) : 'b com =
    (map_iterate f left (left c), f c, map_iterate f right (right c))

  (* An initial state equal to "x0" at origin and "x" elsewhere *)

  let point x0 x = 
    let rec l = lazy(Cell(x,l)) in (l, x0, l)

end

(* Demo *)

let _ =

  let rule (c: bool Cells.com) =
    let left = Cells.proj (Cells.left c)
    and middle = Cells.proj c
    and right = Cells.proj (Cells.right c) in
    not (left && middle && not right || (left = middle)) in

  let step c = Cells.cobind rule c in

  let print c =
    let r = ref c in
    for i = 1 to 20 do r := Cells.left !r done;
    for i = 1 to 40 do
      print_char (if Cells.proj !r then '#' else '.');
      r := Cells.right !r
    done;
    print_newline() in

  let r = ref (Cells.point true false) in
  for i = 1 to 20 do
    print !r; 
    r := step !r
  done
