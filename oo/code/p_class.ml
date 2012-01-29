class ['a] cell(x:'a) = object
  method get = x
end ;;
(** class ['a] cell : 'a -> object method get : 'a end You know, for
    class type signature, no default quantifier any more, so you have
    to suply the 'a variable explicitly
*)


let show l = List.map (fun x -> x#show) l

class integer x = object
  method show = string_of_int x
end

class floating x = object
  method show = string_of_float x
end
class boolean x = object
  method show = string_of_bool x
end

let objs =
  [new integer 10;
   new floating 3.14;
   new boolean true;]

(**
   Here floating, integer, boolean are the same type
   let v :integer = new floating 3.;; is ok
*)    
  
let _ =
  List.iter (Printf.printf "%s\n")  (show objs )


type t = {show : unit -> string}

let show l = List.map (fun x -> x.show ()) l

let integer  x = {show = fun () -> string_of_int   x}
let floating x = {show = fun () -> string_of_float x}
let boolean  x = {show = fun () -> string_of_bool  x}

let _ =
  List.iter
    (Printf.printf "%s\n")
    (show
       [
     integer 10;
         floating 3.14;
         boolean true;
       ]
    )


(** Basically, objects are created faster than records (I think that
    an object is created in O(1) whereas a record takes O(number of
    closures) to be filled). Calls take the same time.
    So, if you have to allocate a great number of values, then I think you
    should consider using objects, otherwise, records wrapping the values
    seem to be a correct option.
*)


type 'x counter = { create : 'x;  get : ('x -> int); inc : ('x -> 'x) }
type 't counter_scope = { bind_counter : 'x. 'x counter -> 't }
type packed_counter = { open_counter : 't. 't counter_scope -> 't }

(* Creating a package from a counter implementation *)
let pack_counter (impl : 'a counter) : packed_counter =
  { open_counter = fun scope -> 
    scope.bind_counter impl }

    

(* Using a package with a scoped expression *)
let with_packed_counter (p:packed_counter) (e:'a counter_scope) : 'a =
  p.open_counter e

(* Two different implementations of the counter *)
let int_impl = {
  create = 1 ;
  get = (function i -> i) ;
  inc = (fun i -> i+1)
}

let float_impl = {
  create = 1.0;
  get = (function i -> (int_of_float i)) ;
  inc = (fun i -> i +. 1.0)
}

let counter = pack_counter int_impl
let counter' = pack_counter float_impl

let expr : int counter_scope =
   { bind_counter = fun counter ->
     (* counter is bound to the << interface >> *)
     (counter.get (counter.inc counter.create)) }

let result = with_packed_counter counter expr
let result' = with_packed_counter counter' expr     


class ['a] foo ch =
object
  method myprintf (fmt : ('a, out_channel, unit) format) =
  Printf.fprintf ch fmt
end
(**
class ['a] foo :
  out_channel ->
  object method myprintf : ('a, out_channel, unit) format -> 'a end
*)
