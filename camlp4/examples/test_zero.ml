(** ocamlbuild -pp 'camlp4o -I _build pa_zero.cmo' test_zero.byte -- *)
let _ = begin
  print_int  (zero + 3 );
end 

  
