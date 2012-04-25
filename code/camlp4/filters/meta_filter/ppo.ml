DEFINE PRINTERS =[
 expr;
 ident;
 patt;
 str_item;
 ctyp;
 match_case;
]

DEFINE PARSERS = [
  expr;
  ident;
  patt;
  str_item;
  ctyp;
  match_case;
]


let _ = __gen__pp__(PRINTERS)

let _ = __gen__parser__ (PARSERS)

(**
   refer ppo.i.mli as reference
 *)    


















