



(*
camlp4o -filter lift test_pa_plus.ml -printer o
  for debugging 
*)
(* let _ = begin  *)
(*   print_int ((!+) (1,2,3,4)); *)
(* end  *)

(**
ocamlbuild -pp 'camlp4o -I _build pa_plus.cmo' test_pa_plus.byte --
10
*)  

let cons x xs = x :: xs

let _ = begin   
  def_foldl ( !* ) ( * );
  def_foldr ( !:: ) ( cons);
end

  
let a = ( !* ) (1, 2, 3, 4) in
let b = ( !::) (1, 2, 3, 34, []) in begin 
print_int a;
end 
  
