let fact n = 
  let rec aux p q n = 
    if n = 0 then p 
    else aux (p+q) p (n-1)
  in aux 1 1 n 
