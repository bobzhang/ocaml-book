type t = semi opaque int

(**
   camlp4o -I _build pa_abstract.cmo test_pa_abstract.ml -abstract -printer o   
   type t
   ocamlbuild -pp 'camlp4o -I _build pa_abstract.cmo -abstract ' test_pa_abstract.cmo
*)    
