
entry module_expr:
  "top"
  "apply"
  "simple"
  [`ANTIQUOT(""|"mexp"|"anti"|"list" as n) s ->
    <:module_expr< $anti_mk_anti ~c:"module_expr" n s$ >>
  |`QUOTATION x ->
      Quotation.expand _loc x Quotation.DynAst.module_expr_tag
  | ... 
  ]

entry str_item:
  "top"
  | value_let; r = opt_rec; bi=binding ->
      << value $rec:r$ $bi$ >>
  | `ANTIQUOT(""|"stri"|"anti"|"list" as n) s ->
      << $anti:mk_anti ~c:"str_item" n s$ >>
  | `QUOTATION x ->
      Quotation.expand _loc x Quotation.DynAst.str_items_tag 


















