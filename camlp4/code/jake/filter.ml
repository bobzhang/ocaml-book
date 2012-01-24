
open BatPervasives
  
module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  value code_of_con_names name cons _loc =
    let match_cases = cons
      |> List.map (fun str -> <:match_case< $uid:str$ -> $str:str$ >>)
      (* |> Ast.mcOr_of_list *)
    in
    let reverse_cases = cons
      |> List.map (fun con -> <:match_case< $str:con$ -> $uid:con$ >>)
      (* |> Ast.mcOr_of_list *)
    in
    <:str_item<
      value $lid:(name^"_to_string") $ =
	  fun [ $list:match_case$ ];
      value $lid:(name^"_of_string") $ =
	  fun [ $list:reserve_cases$ | x -> invalid_arg x ] >>
    >>
 value rec filter str_item = matchp	    
end 
