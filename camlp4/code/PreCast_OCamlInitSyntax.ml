(** Camlp4.PreCast.ml *)
module Id = struct
  value name = "Camlp4.PreCast";
  value version = Sys.ocaml_version;
end;
type camlp4_token = Sig.camlp4_token ==
  [ KEYWORD       of string
  | SYMBOL        of string               (* interesting *)
  | LIDENT        of string
  | UIDENT        of string
  | ESCAPED_IDENT of string               (* interesting *)
  | INT           of int and string
  | INT32         of int32 and string
  | INT64         of int64 and string
  | NATIVEINT     of nativeint and string
  | FLOAT         of float and string
  | CHAR          of char and string
  | STRING        of string and string
  | LABEL         of string
  | OPTLABEL      of string
  | QUOTATION     of Sig.quotation
  | ANTIQUOT      of string and string
  | COMMENT       of string               (* interesting *)
  | BLANKS        of string               (* interesting *)
  | NEWLINE                               (* interesting *)
  | LINE_DIRECTIVE of int and option string  (* interesting *)
  | EOI ];


module Loc = Struct.Loc; 
module Ast = Struct.Camlp4Ast.Make Loc;
module Token = Struct.Token.Make Loc;
module Lexer = Struct.Lexer.Make Token;
module Gram = Struct.Grammar.Static.Make Lexer;
module DynLoader = Struct.DynLoader;
module Quotation = Struct.Quotation.Make Ast;

(** intersting, so you can make your own syntax totally
    but it's not easy to do this in toplevel, probably will crash..
    *)
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


(** Camlp4.OCamlInitSyntax.ml 
    Ast -> Gram -> Quotation -> Camlp4Syntax
    Given Ast, Gram, Quotation, we produce Camlp4Syntax
 *)
Make (Ast:Sig.Camlp4Ast) (Gram:  Sig.Grammar.Static
  with  module Loc = Ast.Loc 
  with type Token.t = Sig.camlp4_token)
  (Quotation : Sig.Quotation
  with module Ast = Sig.Camlp4AstToAst Ast) :Sig.Camlp4Syntax
  with module Loc = Ast.Loc
       module Ast = Ast
       module Gram = Gram
       module Token = Gram.Token
       module Quotation = Quotation
= struct
   ... bla bla
 value a_LIDENT = Gram.Entry.mk "bla bla"
  ...
 EXTEND_Gram
  top_phrase:
    [[ `EOI -> None ]]
  ;
 END;

 module AntiQuoteSyntax = Struct
  module LOC = Ast.Loc
  module Ast = Sig.Camlp4AstToAst Ast ; (** intersting *)
  (** Camlp4AstToAst the functor is a restriction
      functor. Takes a Camlp4Ast module and return it with some
      restrictions
   *)
  module Gram = Gram ;
  value antiquot_expr = Gram.Entry.mk "antiquot_expr";
  value antiquot_patt = Gram.Entry.mk "antiquot_patt";
  EXTEND_Gram
    antiquot_expr :
      [[ x = expr ; `EOI -> x ]] ;
    antiquot_patt :
      [[ x = patt ; `EOI -> x  ]]
  END;
  value parse_expr loc str = Gram.parse_string antiquot_expr loc str ;
  value parse_patt loc str = Gram.parse_string antiquot_patt loc str ;
 end
 module Quotation = Quotation ; 
 value parse_implem ...
 value parse_interf ...
 value print_interf ...
 value print_implem ...
 module Quotation = Quotation ; 
end 






















