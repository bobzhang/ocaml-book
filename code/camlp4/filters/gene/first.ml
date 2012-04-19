

module Id = struct
  value name = "expr";
  value version = Sys.ocaml_version;
end ;
  
module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  open Ast;
  value expr = fun 
     [ <:expr< $x$ + 0 >> | <:expr< 0 + $x$ >> -> x
     | other -> other];
  value map_item ?(str_item= fun x -> x) ?(expr=fun x -> x) () =
  object
    inherit map as super;
    method! str_item = fun x -> str_item (super#str_item x);
    method! expr = fun x -> expr (super#expr x);
  end;
  
  register_str_item_filter
    (map_item ~expr () )#str_item;
end;
  
let module M = Camlp4.Register.AstFilter Id Make in ();
  
  
