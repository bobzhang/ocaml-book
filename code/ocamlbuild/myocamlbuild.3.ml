 (* Open the ocamlbuild world... *)
 open Ocamlbuild_plugin;;
 
 (* We work with commands so often... *)
 open Command;;
 
 (* This dispatch call allows to control the execution order of your
    directives. *)
 dispatch begin function
   (* Add our rules after the standard ones. *)
 | After_rules ->
 
     (* Add pa_openin.cmo to the ocaml pre-processor when use_opening is set *)
     flag ["ocaml"; "pp"; "use_openin"] (A"pa_openin.cmo");
 
     (* Running ocamldep on ocaml code that is tagged with use_openin will require the cmo.
        Note that you only need this declaration when the syntax extension is part of the
        sources to be compiled with ocamlbuild. *)
     dep ["ocaml"; "ocamldep"; "use_openin"] ["pa_openin.cmo"];
 | _ -> ()
 end;;
