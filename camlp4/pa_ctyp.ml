


open Camlp4.PreCast;

module Caml =
  Camlp4OCamlParser.Make
    (Camlp4OCamlRevisedParser.Make
    (Camlp4.OCamlInitSyntax.Make Ast Gram Quotation ));
module Printers = Camlp4.Printers.OCaml.Make Caml;

(** File PreCast.ml

    module Loc = Struct.Loc;
    module Ast = Struct.Camlp4Ast.Make Loc;
    module Token = Struct.Token.Make Loc;
    module Lexer = Struct.Lexer.Make Token;
    module Gram = Struct.Grammar.Static.Make Lexer;
    module DynLoader = Struct.DynLoader;
    module Quotation = Struct.Quotation.Make Ast;
    module MakeSyntax (U : sig end) = OCamlInitSyntax.Make Ast Gram Quotation;
    module Syntax = MakeSyntax (struct end);
    module AstFilters = Struct.AstFilters.Make Ast;
    module MakeGram = Struct.Grammar.Static.Make;

    module Printers = struct
      module OCaml = Printers.OCaml.Make Syntax;
      module OCamlr = Printers.OCamlr.Make Syntax;
     (* module OCamlrr = Printers.OCamlrr.Make Syntax; *)
     module DumpOCamlAst = Printers.DumpOCamlAst.Make Syntax;
     module DumpCamlp4Ast = Printers.DumpCamlp4Ast.Make Syntax;
     module Null = Printers.Null.Make Syntax;
end;
*)

value function_type = "'a -> 'b ";

let printer = (new Printers.printer ()) in
let print_class_str_item = printer#class_sig_item Format.std_formatter in
let print_ctyp = printer#ctyp Format.std_formatter in 
let quant = ["'a"; "'b"] in
let parse_ctyp = Gram.parse_string Caml.ctyp (Loc.mk "<string>") in

let quant : Ast.ctyp =
  List.fold_right
    (fun t acc -> <:ctyp< $parse_ctyp t$ $acc$ >>) quant <:ctyp< >> in
let function_type =
  parse_ctyp function_type in
print_ctyp function_type;
(* print_class_str_item <:class_str_item< method $x$ : ! $list:quant$ . $function_type$ = $expression$ >> ; *)
