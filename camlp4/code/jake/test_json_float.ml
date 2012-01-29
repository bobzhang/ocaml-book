type t = 
    Jq_null 
  |Jq_bool of bool 
  |Jq_number of float 
  |Jq_string of string 
  |Jq_array of t list 
  |Jq_object of (string*t) list 

class map = Camlp4MapGenerator.generated

class fold = Camlp4FoldGenerator.generated
  
(**
   type t =
  | Jq_null
  | Jq_bool of bool
  | Jq_number of float
  | Jq_string of string
  | Jq_array of t list
  | Jq_object of (string * t) list

class map =
  object ((o : 'self_type))
    method string : string -> string = o#unknown
    method list :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
      fun _f_a ->
        function
        | [] -> []
        | _x :: _x_i1 ->
            let _x = _f_a o _x in
            let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
    method float : float -> float = o#unknown
    method bool : bool -> bool = function | false -> false | true -> true
    method t : t -> t =
      function
      | Jq_null -> Jq_null
      | Jq_bool _x -> let _x = o#bool _x in Jq_bool _x
      | Jq_number _x -> let _x = o#float _x in Jq_number _x
      | Jq_string _x -> let _x = o#string _x in Jq_string _x
      | Jq_array _x -> let _x = o#list (fun o -> o#t) _x in Jq_array _x
      | Jq_object _x ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in let _x_i1 = o#t _x_i1 in (_x, _x_i1))
              _x
          in Jq_object _x
    method unknown : 'a. 'a -> 'a = fun x -> x
  end
  
class fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    method float : float -> 'self_type = o#unknown
    method bool : bool -> 'self_type = function | false -> o | true -> o
    method t : t -> 'self_type =
      function
      | Jq_null -> o
      | Jq_bool _x -> let o = o#bool _x in o
      | Jq_number _x -> let o = o#float _x in o
      | Jq_string _x -> let o = o#string _x in o
      | Jq_array _x -> let o = o#list (fun o -> o#t) _x in o
      | Jq_object _x ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#t _x_i1 in o)
              _x
          in o
    method unknown : 'a. 'a -> 'self_type = fun _ -> o
  end
  


*)
