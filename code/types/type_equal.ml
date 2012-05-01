open Format

(** functor can only be modules
    input is a type-constructor
    output is a module type as well 
 *)  

module type Equal = sig
  type fst
  type snd
  module  Coerce (S:sig type 'a f end) : sig 
      val f: fst S.f -> snd S.f
  end 
end 

type ('a,'b)equal = (module Equal with type fst = 'a and type snd = 'b)

module Refl : Equal = struct
  type fst
  type snd = fst
  module Coerce (S:sig type 'a f end) = struct
    let f x = x
  end 
end 

(** parameterize over type is fine
    parameterize over (type constructor) requires functor
    functor, functor also has a signature.
    type declaration appears both in
    module definition and module type definition

    mdoule type declaration appears both in
    module defintion and module type definition
    we can use functor to calculate types....
    It's kinda type-level computation. Fw
 *)
let refl  (type a) = 
  (module struct
    type fst = a
    type snd = a
    module Coerce (S:sig type 'a f end) = struct
      let f x = x 
    end 
  end : Equal with type fst = a and type snd = a)

let a x = x 
















