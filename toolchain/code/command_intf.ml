(** module Command *)
type t =
  |Seq of t list
  (* A sequence of commands (like the `;' in shell) *)
  |Cmd of spec
  (* A command is made of command specifications (spec) *)
  |Echo of string list * pathname
  (* Write the given strings (w/ any formatting) to the given file *)
  |Nop
  (*The type t provides some basic combinators and command
    primitives. Other commands can be made of command specifications
    (spec).*)
type spec =
  |N (*No operation.	*)
  |S of spec list(* A sequence. This gets flattened in the last stages*)
  |A of string	(* An atom.*)
  |P of pathname(* A pathname.*)
  |Px of pathname
  (* A pathname, that will also be given to the call_with_target
     hook. *)
  |Sh of string
  (* A bit of raw shell code, that will not be escaped. *)
  |T of tags
  (* A set of tags, that describe properties and some semantics
     information about the command, afterward these tags will be replaced
     by command specs (flags for instance). *)
  |V of string
  (* A virtual command, that will be resolved at execution using
     resolve_virtuals *)
  |Quote of spec
  (* A string that should be quoted like a filename but isn't really
   one. *)
