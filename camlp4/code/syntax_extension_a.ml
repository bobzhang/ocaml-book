open Camlp4.PreCast
let env = ref []
(** Toploop.toplevel_env *)
(** sucks, in the toplevel, it's really hard to roll back cause, all
    your programs following are affected , horrible *)
let _ = begin
  let _loc = Loc.ghost in 
  EXTEND Gram Syntax.expr:
    LEVEL "simple" [[x = LIDENT -> List.assoc x !env ]] ; END ;
  env := ["x", <:expr< 3 >> ]
end 

let y = 4 in let a = x + y in a;;

(** Error: Camlp4: Uncaught exception: Not_found
   first y,a  is pat
   second y results in an exception 
*)
(** DELETE_RULE Gram Syntax.expr: LIDENT    END ;; *)


(** NOT supported yet
let add_infix lev op = 
    EXTEND Gram 
      Syntax.expr :
      LEVEL $lev$
      [[ x = SELF ;  $op$ ; y = SELF ->
      <:expr< $lid:op$ $x$ $y$ >>]]; END  ;;
*)
