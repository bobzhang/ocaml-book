module type Number = sig
  type t
  val int : int -> t
  val (+) : t -> t -> t
  val (/) : t -> t -> t
end 

let average (type t) number arr = 
  let module N = (val number : Number with type t = t) in 
  N.(
    let r = ref (int 0) and len = Array.length arr in
    for i = 0 to Pervasives.(len - 1) do
      r := !r + arr.(i)
    done;
    !r / int (Array.length arr)
  )

(* val average : (module Number with type t = 'a) -> 'a array -> 'a = <fun>     *)

let f =
  average
    (module struct
      type t = int
      let (+) = (+)
      let (/) = (/)
      let int = fun x -> x
    end : Number with type t = int);;    
(* val f : int array -> int = <fun> *)
