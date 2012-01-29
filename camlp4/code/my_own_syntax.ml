
(** The problem for the toplevel is that you can not find the library
    of the parser?  and
*)
open Camlp4.PreCast; 

module MSyntax=
    (Camlp4OCamlParser.Make
    (Camlp4OCamlRevisedParser.Make
    (Camlp4.OCamlInitSyntax.Make Ast Gram Quotation)));

module OPrinters = Camlp4.Printers.OCaml.Make(MSyntax);
module RPrinters = Camlp4.Printers.OCamlr.Make(MSyntax);


value  parse_exp = MSyntax.Gram.parse_string MSyntax.expr
  (MSyntax.Loc.mk "<string>");

value print_expo =  (new OPrinters.printer ())#expr Format.std_formatter;
value print_expr = (new RPrinters.printer ())#expr Format.std_formatter;
value (|>) x f = f x;

value parse_and_print str = str
  |> parse_exp
  |> (fun x -> begin
    print_expo x;
    Format.print_newline ();
    print_expr x ;
    Format.print_newline ();
  end );

begin
  List.iter parse_and_print
    ["let a = 3 in fun x -> x + 3  ";
     "fun x -> match x with Some  y -> y | None -> 0 ";
    ];
end ;

(**
   output
let a = 3 in fun x -> x + 3
let a = 3 in fun x -> x + 3
fun x -> match x with | Some y -> y | None -> 0
fun x -> match x with [ Some y -> y | None -> 0 ]
*)
