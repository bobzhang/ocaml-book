type a =
  | A of b
  | C
and b =
  | B of a
  | D 

class map = Camlp4MapGenerator.generated

let _ =
  let v = object
    inherit map as super
    method! b x = match super#b x with
      | D -> B C
      | x -> x
  end in
  assert (v#b D = B (C))


    


    
