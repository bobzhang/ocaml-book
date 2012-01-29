type animal = < eat : unit; v : exn >
type dog = < bark : unit; eat : unit; v : exn >
type cat = < eat : unit; meow : unit; v : exn >
exception Dog of dog
exception Cat of cat 

let fido : dog = object(self)
  method v=Dog self
  method eat = ()
  method bark = ()
end;;

let miao : cat = object(self)
  method v = Cat self
  method eat = ()
  method meow = ()
end;;

let _ = begin
  let test o = match o#v with
    | Dog o' -> print_endline "Dog"
    | Cat o' -> print_endline "Cat"
    | _ -> print_endline "not handled"
  in
  test fido;
  test miao;
end 
(**
   Dog
   Cat
*)
