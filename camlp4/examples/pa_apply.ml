open Camlp4.PreCast
let _ =
  AstFilters.register_str_item_filter
  (Ast.map_expr
    (function
      <:expr@loc< $e1$ & $e2$ >> -> <:expr@loc< $e1$ $e2$ >>
     | e -> e ))#str_item
