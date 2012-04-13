(* Author: bobzhang1988@vpl472.wlan.library.upenn.edu        *)
(* Version: $Id: pa_printer.ml,v 0.0 2012/02/15 18:19:14 bobzhang1988 Exp $ *)
open Printf

module Id = struct
  let name = "Printer How to"
  let version = "$Id: how to version 1$ "
end

let (^$) f x = f x

(** Camlp4.Sig.Syntax.Ast is not very powerful,
    many interfaces are not exposed
*)  
module Make (Syntax: Camlp4.Sig.Syntax) :
  Camlp4.Sig.Printer(Syntax.Ast).S = struct
    module Ast = Syntax.Ast
    let opt_string = function
      | None -> "<None>"
      | Some s -> s
    let info ?input_file ?output_file name =
      eprintf
	"printer on %s\n input: %s\n output: %s\n"
	name
	(opt_string input_file)
	(opt_string output_file)
    let print_interf ?input_file ?output_file (ast:Ast.sig_item) = begin 
      (* let n = List.length ^$ Ast.list_of_str_item ast [] in  *)
      info ?input_file ?output_file "signature";
      (* eprintf "%d sig_items\n"; *)
    end 
    let print_implem ?input_file ?output_file (ast:Ast.str_item) = begin 
      (* let n = List.length ^$ Ast.list_of_str_item ast [] in *)
      info ?input_file ?output_file "structure";
      (* eprintf "%d str_items\n"; *)
    end 
  end  
module M = Camlp4.Register.Printer(Id) (Make)

(**
   camlp4o -I _build  pa_printer.cmo pa_printer.ml   

 printer on structure
 input: pa_printer.ml
 output: <None>

*)  


















