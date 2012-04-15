open Ocamlbuild_plugin
let () =
  dispatch begin function
  | After_rules ->
    dep ["myfile"] ["other.ml"]
  | _ -> ()
  end	
