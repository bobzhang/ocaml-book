type 'a animal = <eat:unit; tag : 'a >;;


let fido : [< `Dog of int] animal = object method eat = () method tag = `Dog 3 end;;
(* val fido : [ `Dog of int ] animal = <obj> *)


let fido : 'a animal = object method eat = () method tag = `Dog 3 end;;

(* val fido : [> `Dog of int ] animal = <obj> *)




let miao : [> `Cat of int] animal = object method eat = () method tag = `Cat 2 end;;
(* val miao : [> `Cat of int ] animal = <obj> *)
let aims =  [fido;miao];;
(* [> `Cat of int | `Dog of int ] animal list = [<obj>; <obj>] *)


List.map (fun v -> match v#tag with `Cat a -> a |`Dog a -> a) [fido;miao];;
(* - : int list = [3; 2] *)

