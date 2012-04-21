(* Version: $Id: util.ml,v 0.0 2012/02/18 05:20:48 bobzhang1988 Exp $ *)
open Camlp4.PreCast;
open Printf;
module StringMap = Map.Make(String);

value (|>) x f = f x;
value (^$) f x = f x;
value failwithf fmt = ksprintf failwith fmt  ;
value prerr_endlinef fmt = ksprintf prerr_endline fmt;

(** fold [0,n)  *)    
value  fold_nat_left f acc n =
    let rec aux acc m = 
      if m > n then invalid_arg "fold_nat"
      else if m = n then acc 
      else aux (f acc m) (m+1) in 
    aux acc 0 ;

value ends_with s e = 
   let ne = String.length e
   and ns = String.length s in
   ns >= ne && String.sub s (ns-ne) ne = e ;

value rec list_of_expr_list (e:Ast.expr) :  list Ast.expr =
  match e with
  [ <:expr< [] >> -> []
  | <:expr< [$b$::$bs$ ] >>  -> [b :: list_of_expr_list bs ]
  | _ -> invalid_arg "list_of_expr_list"]
;	
   
