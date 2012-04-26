open Camlp4.PreCast;
open Format;
module StringMap = Map.Make(String);

value (|>) x f = f x;
value (&) f x = f x;
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

module Make(A:Camlp4.Sig.AstFilters) = struct
  open A;
  value rec list_of_expr_list (e:Ast.expr) :  list Ast.expr =
    match e with
   [ <:expr< [] >> -> []
   | <:expr< [$b$::$bs$ ] >>  -> [b :: list_of_expr_list bs ]
   | _ -> invalid_arg "list_of_expr_list"]
;	

  value parser_of_entry entry  s =
  try Gram.parse entry (Loc.mk "<string>") (Stream.of_string  s)
  with
    [Loc.Exc_located(loc, e) -> begin 
      prerr_endline (Loc.to_string loc);
      let (start_bol,stop_bol,
        start_off, stop_off) =
        Loc.( (start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc)
            ) in
      let abs_start_off = start_bol + start_off in
      let abs_stop_off = stop_bol + stop_off in
      let err_location = String.sub s abs_start_off
          (abs_stop_off - abs_start_off + 1) in
      prerr_endline (sprintf "err: ^%s^" err_location);
      raise e ;
    end ]
;

  (** here we pattern match
      one kind of module_expr
      [MeStr of loc and str_item]

      there are other kinds of module expression
      but not very useful for camlp4, since it's
      need syntax level to process
      or generate code
   *)  
  value module_hook (e:Ast.str_item) f =
    match e with 
   [ <:str_item@_loc<
     module $name$ = struct $stri:body$ end
         >> ->
           f _loc name body
    | x -> x           
   ];

      
end;
  
