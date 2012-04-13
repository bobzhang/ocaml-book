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
   
(**
DEFINE FMT(x) =  x Format.std_formatter ;
DEFINE SFMT(x,y)  =
  let buf = Buffer.create 50 in
  let fmt = Format.formatter_of_buffer buf in begin 
    x fmt y;
    Buffer.contents buf
  end 
;
module OPrinters = Camlp4.Printers.OCamlr.Make(Syntax);
value op = (new OPrinters.printer());
value p_expr   =  FMT (op#expr);
value p_ident =  FMT (op#ident);
value p_patt  =  FMT (op#patt) ;
value p_str_item  =  FMT (op#str_item);
value p_ident  =  FMT (op#ident);
value p_ctyp =  FMT (op#ctyp);
value p_match_case =  FMT (op#match_case);
value s_expr  e =  SFMT(op#expr,e);
value s_ident e =  SFMT (op#ident,e);
value s_patt  e =  SFMT (op#patt,e) ;
value s_str_item e =  SFMT (op#str_item,e);
value s_ident e =  SFMT (op#ident,e);
value s_ctyp e=  SFMT (op#ctyp,e);
value s_match_case e=  SFMT (op#match_case,e);
*)   
