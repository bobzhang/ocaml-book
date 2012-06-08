open Format

(* unpacking *)  
let sort (type s) set l =
  let module Set = (val set : Set.S with type elt = s) in
  Set.elements (List.fold_right Set.add l Set.empty)

(* packing*)    
let make_set (type s) cmp =
  let module S = Set.Make(struct
    type t = s
    let compare = cmp
   end) in
  (module S:Set.S with type elt = s)

module type S = sig
  type t=int
  module  X : sig type u   end
end

let f ( module X : S)  (y:X.X.u) =
  3















