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
