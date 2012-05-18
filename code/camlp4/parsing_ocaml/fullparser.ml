open Camlp4.PreCast ;
open Batteries_uni;
module MySyntax = Camlp4.OCamlInitSyntax.Make Ast Gram Quotation ;
module M = Camlp4OCamlRevisedParser.Make MySyntax ;
module M4 = Camlp4QuotationExpander.Make MySyntax ;

value my_parser = MySyntax.parse_implem;
value str_items_of_file file_name = 
  file_name
  |> open_in  
  |> BatStream.of_input
  |> my_parser (Loc.mk file_name)
  |> flip Ast.list_of_str_item [] ; (** destruct 1*)

value rec do_str_item str_item tags = 
  match str_item with 
  [ <:str_item< value $rec:_$ $binding$ >> ->  (** destruct 2*)
        let bindings = Ast.list_of_binding binding []
        in List.fold_right do_binding bindings tags 
  |_ -> tags ]
and do_binding bi tags =
  match bi with 
  [ <:binding@loc< $lid:lid$ = $_$ >> -> (** destruct 3*)
    let line = Loc.start_line loc in 
    let off = Loc.start_off loc in 
    let pre = "let " ^ lid in 
    [(pre,lid,line,off) :: tags ]
  | _ -> tags ];  

value do_fn file_name = 
    file_name 
    |> str_items_of_file 
    |> List.map (flip do_str_item [])
    |> List.concat ; 

value _ = 
  do_fn "fullparser.ml"
  |> List.iter (fun (a, b, c, d) -> Printf.printf "%s-%s %d-%d \n" a b c d)  ; 

