 open Ocamlbuild_plugin;;
 open Command;;
 dispatch begin function
   | After_rules ->
       (* Add pa_openin.cmo to the ocaml pre-processor when use_opening is set *)
       flag ["ocaml"; "pp"; "use_openin"] (A"pa_openin.cmo");
       (* Running ocamldep on ocaml code that is tagged with use_openin will require
	  the cmo.  Note that you only need this declaration when the syntax extension
	  is part of the  sources to be compiled with ocamlbuild. *)
       dep ["ocaml"; "ocamldep"; "use_openin"] ["pa_openin.cmo"];
   | _ -> ()
 end;;
