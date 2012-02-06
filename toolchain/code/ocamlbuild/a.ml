
open Ocamlbuild_plugin
open Printf
let _ = dispatch (function
  | Before_options ->
    printf "Before options the -ocamlc option is set to %s.\n"
)
