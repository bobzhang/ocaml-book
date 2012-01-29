open Maybe

let lift f x y = 
  try Succ (f x y) with e -> Fail e

let comparen l1 l2 n =
  { try
      { let! x = lift List.nth l1 n in
        let! y = lift List.nth l2 n in
        return x = y }
    with e -> return false }

let to_string = 
  function Succ b -> string_of_bool b | Fail e -> raise e

let _ =
  print_endline (to_string (comparen [] [1;2;3] 2)); 
  print_endline (to_string (comparen [1;2;3] [] 2)); 
  print_endline (to_string (comparen [1;2;3] [1;2;3] 2)); 
  print_endline (to_string (comparen [1;2;3] [3;2;1] 2))


