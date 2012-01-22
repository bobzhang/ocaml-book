

type (_,_) eq =
  | Refl : ('a,'a) eq


let sym : ('a,'b) eq -> ('b,'a)eq =
  function Refl -> Refl

let trans :('a,'b) eq
    -> ('b,'a) eq
    -> ('a,'c) eq = fun Refl Refl
    -> Refl

module type TC = sig
  type 'a tc
end

  
  
module type SUBST= functor (T : TC) ->
sig
  val subst :
    ('a,'b) eq ->
    ('a T.tc,'b T.tc ) eq
end 

module Subst  = functor (T:TC) -> struct
  let subst  : 'a 'b . ('a,'b) eq -> ('a T.tc,'b T.tc)eq = fun
    Refl ->  Refl
end

  
let cong  (type t) (Refl: ('a,'b) eq)
    : ('a t,'b t) eq =
  Subst (struct type 'a tc = 'a eq)


let cong : 'a 'b . ('a,'b) eq ->  ('a list,'b list) eq = fun 
  Refl -> Refl
