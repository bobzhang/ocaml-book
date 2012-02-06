module type OPTIONS = sig
  type command_spec

  val build_dir : string ref
  val include_dirs : string list ref
  val exclude_dirs : string list ref
  val nothing_should_be_rebuilt : bool ref
  val ocamlc : command_spec ref
  val ocamlopt : command_spec ref
  val ocamldep : command_spec ref
  val ocamldoc : command_spec ref
  val ocamlyacc : command_spec ref
  val ocamllex : command_spec ref
  val ocamlrun : command_spec ref
  val ocamlmklib : command_spec ref
  val ocamlmktop : command_spec ref
  val hygiene : bool ref
  val sanitize : bool ref
  val sanitization_script : string ref
  val ignore_auto : bool ref
  val plugin : bool ref
  val just_plugin : bool ref
  val native_plugin : bool ref
  val make_links : bool ref
  val nostdlib : bool ref
  val program_to_execute : bool ref
  val must_clean : bool ref
  val catch_errors : bool ref
  val use_menhir : bool ref
  val show_documentation : bool ref
  val recursive : bool ref
  val use_ocamlfind : bool ref

  val targets : string list ref
  val ocaml_libs : string list ref
  val ocaml_mods : string list ref
  val ocaml_pkgs : string list ref
  val ocaml_cflags : string list ref
  val ocaml_lflags : string list ref
  val ocaml_ppflags : string list ref
  val ocaml_yaccflags : string list ref
  val ocaml_lexflags : string list ref
  val program_args : string list ref
  val ignore_list : string list ref
  val tags : string list ref
  val tag_lines : string list ref
  val show_tags : string list ref

  val ext_obj : string ref
  val ext_lib : string ref
  val ext_dll : string ref
  val exe : string ref

  val add : string * Arg.spec * string -> unit
end
