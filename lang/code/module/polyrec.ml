(* Six encodings of polymorphic recursion in OCaml 3.12, including two
   "new" encodings using first-class modules. *)

(* As an example, we'll consider the problem of summing the leaves of a
   perfect tree, expressed as a nested data type *)
type 'a perfect = Zero of 'a | Succ of 'a fork perfect
and 'a fork = Fork of 'a * 'a

(* 1. OCaml 3.12 introduces quantifiers for type signatures attached
   to let bindings, allowing us to write polymorphic-recursive
   functions directly. *)
let rec sump_direct : 'a. ('a -> int) -> 'a perfect -> int =
  fun f -> function
    | Zero x -> f x
    | Succ x -> sump_direct (fun (Fork (a,b)) -> f a + f b) x

let sum_perfect_direct = sump_direct (fun x -> x)

(* 2. Polymorphic fields in records can be used to give the
   signature. *)
type sump_record = {
  sump: 'a. ('a -> int) -> 'a perfect -> int
}
let rec sump_record : sump_record = {
  sump = fun f -> function
    | Zero x -> f x
    | Succ x -> sump_record.sump (fun (Fork (a,b)) -> f a + f b) x
}
let sum_perfect_record = sump_record.sump (fun x -> x)

(* 3. Polymorphic methods in objects can be used to give the
   signature.  (We can also take advantage of the implicit recursion
   in the self binding.) *)
let sump_object =
object (self)
  method sump : 'a. ('a -> int) -> 'a perfect -> int
    = fun f -> function
      | Zero x -> f x
      | Succ x -> self#sump (fun (Fork (a,b)) -> f a + f b) x
end

let sum_perfect_object = sump_object#sump (fun x -> x)
  
(* 4. Type variables in module signatures are "rigid", so recursive
   modules also offer polymorphic recursion. *)
let sump_module x =
  let module M =
    struct
      module rec SumP :
      sig
        val sump : ('a -> int) -> 'a perfect -> int
      end =
      struct
        let rec sump =
          fun f -> function
            | Zero x -> f x
            | Succ x -> SumP.sump (fun (Fork (a,b)) -> f a + f b) x
      end
    end in M.SumP.sump x

let sum_perfect_module = sump_module (fun x -> x)

(* 5. First-class modules introduce more ways to introduce polymorphic
   recursion.  Here's an explicit encoding in which we use a functor
   to encode "big lambda". *)
module type SUMP = 
  functor (S : sig type a end) ->
sig
  val sump : (S.a -> int) -> S.a perfect -> int
end

let rec sump_fcm_explicit : (module SUMP) =
  (module
     functor (S : sig type a end) ->
     struct
       let sump f = function
         | Zero x -> f x
         | Succ x -> 
             let module M = 
                 struct
                   module SS = (val sump_fcm_explicit  : SUMP) 
                   module SS' = SS(struct type a = S.a fork end)
                 end in 
               M.SS'.sump (fun (Fork (a,b)) -> f a + f b) x
     end : SUMP)

let sum_perfect_fcm_explicit = 
  let module M =
      struct
        module S = (val sump_fcm_explicit : SUMP)
        module S' = S(struct type a = int end)
      end in M.S'.sump (fun x -> x)

(* 6. We don't have to encode the type abstraction explicitly, though.
   Here's a second version using first-class modules that simply takes
   advantage of the fact that type variables in module signatures are
   "rigid". *)
module type SUMP' = 
sig
  val sump : ('a -> int) -> 'a perfect -> int
end

let rec sump_fcm_implicit : (module SUMP') =
  (module
   struct
     let sump f = function
       | Zero x -> f x
       | Succ x -> 
           let module SS = (val sump_fcm_implicit  : SUMP') in 
             SS.sump (fun (Fork (a,b)) -> f a + f b) x
   end : SUMP')
    
let sum_perfect_fcm_implicit =
  let module S = (val sump_fcm_implicit : SUMP') in
    S.sump (fun x -> x)


(* A simple test *)
let _ =
  let fork a b = Fork (a, b) in
  let perfect = Succ (Succ (Succ (Zero (fork (fork
                                                (fork 2 3)
                                                (fork 5 7))
                                          (fork 
                                             (fork 11 13)
                                             (fork 17 19))))))
  in
    begin
      assert (sum_perfect_fcm_implicit perfect = 77);
      assert (sum_perfect_fcm_explicit perfect = 77);
      assert (sum_perfect_module perfect = 77);
      assert (sum_perfect_object perfect = 77);
      assert (sum_perfect_record perfect = 77);
      assert (sum_perfect_direct perfect = 77);
    end
