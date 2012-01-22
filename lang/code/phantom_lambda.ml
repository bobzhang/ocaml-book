



module type S = sig
  type 'a expr
  val int : int -> int expr
  val bool : bool -> bool expr
  val add : int expr -> int expr -> int expr
  val app : ('a -> 'b ) expr -> ('a expr -> 'b expr)
  val lam : ('a expr -> 'b expr ) -> ('a -> 'b) expr 
end



module M : S= struct   
  type term =
      Int of int
    | Bool of bool
    | Add of term * term
    | App of term * term
    | Lam of (term -> term)

  type 'a expr =  term (* The phantom type *)
  let int : int -> int expr = fun i ->  (Int i)
  let bool : bool -> bool expr = fun b ->  (Bool b)

  let add : int expr -> int expr -> int expr =
    fun ( e1) ( e2) ->  (Add(e1,e2))

  let app : ('a -> 'b) expr -> ('a expr -> 'b expr) =
    fun ( e1) ( e2) ->  (App(e1,e2))

  let lam : ('a expr -> 'b expr) -> ('a -> 'b) expr =
    fun f ->  (Lam(fun x -> let ( b) = f ( x) in b))
end 


open M

(*
  lam (fun x -> app x x );;
                      ^
Error: This expression has type ('a -> 'b) M.expr
       but an expression was expected of type 'a M.expr
*)
