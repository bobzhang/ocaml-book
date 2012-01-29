


let list_obj initial = object
  val content = initial
  method cons x = {< content = x :: content >}
end 


(** module style *)  
module type PolySig = sig
  type poly
  val create : (float*float) array -> poly
  val draw : poly -> unit
  val transform : poly -> poly 
end
;;  

module Poly :PolySig = struct 
  type poly = (float * float) array
  let create vertices = vertices
  let draw vertices = ()
  let transform matrix = matrix 
end;;
  

(** class style *)  
class type poly  = object
 method create : (float*float) array -> poly
 method draw :  unit
 method transform : poly
end;;
  
class poly_class  = object (self:'self)
  val mutable vertices : (float * float ) array = [||]
  method create vs = {< vertices = vs >}
  method draw = ()
  method transform = {< vertices = vertices >}
end;;

(** makes the type not that horrible. First class objects, but not
    first class classes
*)
let a_obj : poly = new poly_class


(** oo-style *)  
type blob = < draw : unit -> unit; transform : unit -> blob >;;

(** functional style *)
type blob2 = {draw:unit-> unit; transform:unit-> blob2};;



