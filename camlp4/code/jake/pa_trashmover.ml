open Camlp4;

module Id = struct
  value name    = "Camlp4TrashRemover";
  value version = Sys.ocaml_version;
end;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  open Ast;

  register_str_item_filter
    (Ast.map_str_item
      (fun
       [ <:str_item@_loc< module Camlp4Trash = $_$ >> ->
            <:str_item<>>
       | st -> st ]))#str_item;

end;

let module M = Camlp4.Register.AstFilter Id Make in ();
