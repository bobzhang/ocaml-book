type _ expr = 
  | Int : int -> int expr
  | Add : (int -> int -> int) expr
  | App : ('a -> 'b ) expr * 'a expr -> 'b expr


let rec eval : type t . t expr -> t = function
  | Int n -> n
  | Add -> (+)
  | App (f,x) -> eval f (eval x)


(** tagless data structure *)    
type _ ty =
  | Tint : int ty
  | Tbool : bool ty
  | Tpair : 'a ty * 'b ty -> ('a * 'b) ty

(** inside pattern matching, type inference progresses from left to
    right, allowing subsequent patterns to benift from type equations
    generated in the previous ones.
    This implies that d has type int on the first line,...
*)
let rec print : type a . a ty -> a -> string = fun t d ->
  match t, d with
    |Tint, n -> string_of_int n
    |Tbool,true -> "true"
    |Tbool,false -> "false"
    |Tpair (ta,tb), (a,b) ->
      "(" ^ print ta a ^ ", " ^ print tb b ^ ")"

let f = print (Tpair (Tint,Tbool))

