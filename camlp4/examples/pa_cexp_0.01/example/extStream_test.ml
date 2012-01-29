open ExtStream

let sieve n s = 
  { let! x = s in
    if not (x mod n = 0 ) then return x }

let rec prime s =
  {let! h = s in
   return h;
   return! prime (sieve h s)}

let _ =
  Stream.iter print_int (prime (2 -- 100))

