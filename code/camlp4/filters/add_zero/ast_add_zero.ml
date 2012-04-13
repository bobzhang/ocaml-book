open Camlp4.PreCast;
let simplify = object
  inherit Ast.map as super;
  method expr e =
    match super#expr e with
    [ <:expr< $x$ + 0 >> | <:expr< 0 + $x$  >> ->  x
    | x -> x ];
end in AstFilters.register_str_item_filter simplify#str_item
;

(** the same as above without syntax extension, you can get with
    camlp4of ast_add_zero.ml -printer o 
let _ =
  let simplify =
object
  inherit Ast.map as super
  method expr =
    fun e ->
      match super#expr e with
        | Ast.ExApp (_,
		     (Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "+")))), x)),
		     (Ast.ExInt (_, "0"))) |
            Ast.ExApp (_,
                       (Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "+")))),
				   (Ast.ExInt (_, "0")))),
                       x)
          -> x
        | x -> x
end
  in AstFilters.register_str_item_filter simplify#str_item
*)


      

      






















