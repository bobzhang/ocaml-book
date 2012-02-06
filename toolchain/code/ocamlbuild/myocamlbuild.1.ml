
open Ocamlbuild_plugin
open Printf


let _ = dispatch (function
  | Before_options ->
    printf "Before options the -ocamlc option is set to %s.\n"
      (Command.string_of_command_spec !Options.ocamlc);
    tag_file "bla.ml" ["use_unix"];
    Tags.print Format.std_formatter (tags_of_pathname "bla.ml")
  | After_options ->
    printf "Before options the -ocamlc option is set to %s.\n"
      (Command.string_of_command_spec !Options.ocamlc);
    tag_file "bla.ml" ["-use_unix"];
    Tags.print Format.std_formatter (tags_of_pathname "bla.ml")
  | _ -> ()
)
