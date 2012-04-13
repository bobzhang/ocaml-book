open Camlp4.PreCast ; 
open BatPervasives ;
module MySyntax = Camlp4.OCamlInitSyntax.Make Ast Gram Quotation ;
module M = Camlp4OCamlRevisedParser.Make MySyntax ;
(** load quotation parser *)
module M4 = Camlp4QuotationExpander.Make MySyntax ;

(** in toplevel, I did not find a way to introduce such module
    because it will change the state 
    module N = Camlp4OCamlParser.Make MySyntax ;
    load o parser
*)

value my_parser = MySyntax.parse_implem;
value str_items_of_file file_name = 
  file_name
  |> open_in  
  |> BatStream.of_input
  |> my_parser (Loc.mk file_name)
  |> flip Ast.list_of_str_item [] ;

(** it has ambiguity in original syntax, so pattern match 
    will be more natural in revised syntax 
*)
value rec do_str_item str_item tags = 
  match str_item with 
  [ <:str_item< value $rec:_$ $binding$ >> -> 
        let bindings = Ast.list_of_binding binding []
        in List.fold_right do_binding bindings tags 
  |_ -> tags ]
and do_binding bi tags =
  match bi with 
  [ <:binding@loc< $lid:lid$ = $_$ >> -> 
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
(**use MSyntax.parse_implem*)
value _ = 
  do_fn "parser.ml"
  |> List.iter (fun (a, b, c, d) -> Printf.printf "%s-%s %d-%d \n" a b c d)  ; 

(** output
   let my_parser-my_parser 14-423 
   let str_items_of_file-str_items_of_file 15-464 
   let do_str_item-do_str_item 25-733 
   let do_binding-do_binding 31-958 
   let do_fn-do_fn 41-1204 
*)
