open Camlp4.PreCast

let s = object
  inherit Ast.map as super
  method! expr e  = match super#expr e with
    | <:expr< zero + $a$ >> -> a
    | <:expr< $a$ + zero >> -> a
    | _  as x ->  x
end 
in AstFilters.register_str_item_filter s#str_item

(**
open Camlp4.PreCast
  
let _ =
  let s =
    object
      inherit Ast.map as super
      method! expr =
        fun e ->
          match super#expr e with
          | Ast.ExApp (_,
              (Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "+")))),
                 (Ast.ExId (_, (Ast.IdLid (_, "zero")))))),
              a) -> a
          | Ast.ExApp (_,
              (Ast.ExApp (_, (Ast.ExId (_, (Ast.IdLid (_, "+")))), a)),
              (Ast.ExId (_, (Ast.IdLid (_, "zero"))))) -> a
          | (_ as x) -> x
    end
  in AstFilters.register_str_item_filter s#str_item
  


*)
