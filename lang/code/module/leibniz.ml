module type TyCon = sig
  type 'a tc
end


module type WeakEQ =
sig
  type ('a, 'b) eq
  val refl : unit -> ('a, 'a) eq
  val symm : ('a, 'b) eq -> ('b, 'a) eq
  val trans : ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq
  val cast : ('a, 'b) eq -> 'a -> 'b
end

module WeakEq : WeakEQ =
struct
  type ('a, 'b) eq = ('a -> 'b) * ('b -> 'a)
  let refl () = (fun x -> x), (fun x -> x)
  let symm  (f, g)        = (g, f)
  let trans (f, g) (j, k) = (fun x -> j (f x)), (fun x -> g (k x))
  let cast  (f, g)        = f
end

module type EQ =
sig
  type ('a, 'b) eq
  val refl : unit -> ('a, 'a) eq

  module Subst (TC : TyCon) :  sig
    val subst : ('a, 'b) eq -> ('a TC.tc, 'b TC.tc) eq
  end
  val cast : ('a, 'b) eq -> 'a -> 'b
end

module Eq : EQ = struct

  (** EqTC can be seen as a high-order kind, parameterized by two type
      variables a b. This is the limitation of ocaml, since type
      variable as a parameter can only appear in [type 'a t], the type
      variable will be *universally quantified* when it appears in
      other places *)
  module type EqTC =  sig
    type a
    type b
    (** You see the definition of [TC], it could be parameterized
	here *)
    module Cast : functor (TC : TyCon) ->  sig
      val cast : a TC.tc -> b TC.tc
    end
  end 

  type ('a, 'b) eq = (module EqTC with type a = 'a and type b = 'b)

  let refl (type t) () =  (module struct
    type a = t
    type b = t
    module Cast (TC : TyCon) =
    struct
      let cast v = v
    end
  end : EqTC with type a = t and type b = t)

  let cast (type s) (type t) s_eq_t =
    let module S_eqtc = (val s_eq_t : EqTC with type a = s and type b = t) in
    let module C = S_eqtc.Cast(struct type 'a tc = 'a end) in
    C.cast

  module Subst (TC : TyCon) =  struct
    (** We have (s,t) eq, now we want to construct a proof of (s TC.t,
	t Tc.t) eq .
	i.e, a Sc.t -> b Sc.t, s Tc.t Sc.t -> t Tc.t Sc.t *)
    let subst (type s) (type t) s_eq_t =
      (module
       struct
	 type a = s TC.tc
	 type b = t TC.tc
	 module S_eqtc = (val s_eq_t : EqTC with type a = s and type b = t)
	 module Cast (SC : TyCon) =
	 struct
	   module C = S_eqtc.Cast(struct type 'a tc = 'a TC.tc SC.tc end)
	   let cast = C.cast
	 end
       end : EqTC with type a = s TC.tc and type b = t TC.tc)
  end
end

include Eq

let symm : 'a 'b. ('a, 'b) eq -> ('b, 'a) eq =
  fun (type a) (type b) a_eq_b ->
    let module S = Subst (struct type 'a tc = ('a, a) eq end) in
    cast (S.subst a_eq_b) (refl ())


let trans : 'a 'b 'c. ('a, 'b) eq -> ('b, 'c) eq -> ('a, 'c) eq =
  fun (type a) (type b) (type c) a_eq_b b_eq_c ->
    let module S = Subst(struct type 'a tc = (a, 'a) eq end) in
    cast (S.subst b_eq_c) a_eq_b

(** Our implementation of equality seems sufficient for the common
    examples, but has one apparent limitation, described below.  A few
    examples seem to require an inverse of Leibniz's law.  For
    injectivty type constructors t, we would like to have ('a t, 'b t)
    eq -> ('a, 'b) eq For example, given a proof that two function
    types are equal, we would like to extract proofs that the domain
    and codomain types are equal: ('a -> 'b, 'c -> 'd) eq -> ('a, 'c)
    eq * ('b, 'd) eq GADTs themselves support type decomposition in
    this way.  Unfortunately, injectivity is supported only for
    WeakEq.eq.  We may always get WeakEq.eq from EQ.eq.
*)
let degrade : 'r 's. ('r, 's) eq -> ('r, 's) WeakEq.eq =
  fun (type r) (type s) r_eq_s ->
    let module M = Eq.Subst(struct type 'a tc = ('a, r) WeakEq.eq end) in
    WeakEq.symm (cast (M.subst r_eq_s) (WeakEq.refl ()))

