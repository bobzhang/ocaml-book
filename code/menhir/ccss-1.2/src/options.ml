(********************************************************************************)
(*	Options.ml
	Copyright (c) 2010 Dario Teixeira (dario.teixeira@yahoo.com)
	This software is distributed under the terms of the GNU GPL version 2.
	See LICENSE file for full license text.
*)
(********************************************************************************)
open Batteries
open OptParse


(********************************************************************************)
(**	{1 Functions and values}						*)
(********************************************************************************)

let options = OptParser.make ()

let convert_opt = StdOpt.store_true ()


let () =
	let general = OptParser.add_group options "General options"
	in OptParser.add options ~group:general ~short_name:'c' ~long_name:"convert" ~help:"Attempt unit conversion" convert_opt


let parse () = match OptParser.parse_argv options with
	| hd::tl ->
		OptParser.usage options ();
		OptParser.error options "Error: invalid usage";
		raise Exit
	| [] ->
		Opt.get convert_opt

