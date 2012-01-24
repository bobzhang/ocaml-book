type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
module Show_tree :
  functor (M_a : Deriving_Show.Show) ->
    sig
      type a = M_a.a tree
      val format : Format.formatter -> a -> unit
      val format_list : Format.formatter -> a list -> unit
      val show : a -> string
      val show_list : a list -> string
    end
module Eq_tree :
  functor (M_a : Deriving_Eq.Eq) ->
    sig type a = M_a.a tree val eq : a -> a -> bool end
module Typeable_tree :
  functor (M_a : Deriving_Typeable.Typeable) ->
    sig
      type a = M_a.a tree
      val type_rep : Deriving_Typeable.TypeRep.t Lazy.t
      val has_type : Deriving_Typeable.dynamic -> bool
      val cast : Deriving_Typeable.dynamic -> a option
      val throwing_cast : Deriving_Typeable.dynamic -> a
      val make_dynamic : a -> Deriving_Typeable.dynamic
      val mk : a -> Deriving_Typeable.dynamic
    end
module rec Functor_tree :
  sig type 'a f = 'a tree val map : ('a -> 'b) -> 'a tree -> 'b tree end
