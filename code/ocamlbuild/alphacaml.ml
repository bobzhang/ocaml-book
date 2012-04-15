open Ocamlbuild_plugin;;
open Command;;

let alphaCaml = A"alphaCaml";;

dispatch begin function
  | After_rules ->
      rule "alphaCaml: mla -> ml & mli"
        ~prods:["%.ml"; "%.mli"]
        ~dep:"%.mla"
	begin fun env _build ->
          Cmd(S[alphaCaml; P(env "%.mla")])
	end
  | _ -> ()
end
(**
   Extra efforts:
   The pointed directory contains the compiled files (.cmo, .cmi).
   $ ln -s /path/to/your/alphaCaml/directory/ alphaLib
   $ cat _tags
   "alphaLib": include, precious
   it's very nice to make the whole directory precious, this is a way to mix 
   # different buiding unit.   
 *)
