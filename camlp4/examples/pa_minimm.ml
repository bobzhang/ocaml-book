

open Camlp4.PreCast;
open Syntax;

value mmstatement = Gram.Entry.mk "mmstatement";
value mmvalue = Gram.Entry.mk "mmvalue";
value mmtest = Gram.Entry.mk "mmtest";
value mmident = Gram.Entry.mk "mmident";

value rec compile _loc tag e = fun [
    [ ] ->  <:binding< $lid:tag$ = fun () -> $e$ >>
  | [`TL e' ; `T tag' :: t] ->
    let bs = compile _loc tag' <:expr< () >>
  ]
  
