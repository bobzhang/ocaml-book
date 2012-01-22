

let a : bool = 17
  |> flip Marshal.to_string []
  |> flip Marshal.from_string 0

(* val a : bool = <unknown constructor>       *)
      
let test ()
    = print_bool  a

let _ = test ()
(* true- : unit = ()   *)

let _ =
  assert ((match a with true -> 1 | false -> 2) = 1)


let marshal_id v = v
  |>  flip Marshal.to_string []
  |> flip Marshal.from_string 0


type x  = A       

let _ = begin
  let a : bool = marshal_id None in
  print_bool a ; (* false *)
  let b : bool = marshal_id A in
  print_bool b ;
end


module M = struct
    type t = int
    let x = 3
    let p () = print_int x
end  

module M1 : sig
  type t
  val x : t
  val p : unit -> unit
end = M

let c : M1.t = marshal_id M.x
  
