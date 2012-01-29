



open Camlp4;
open Camlp4.PreCast;


(* AstFilters.register_str_item_filter (Ast.map_expr begin fun *)
(*   [ *)
(*     (\* <:expr< ( $tup:e$ ) >> *\) *)
(*     <:expr@_loc< (!+) ($tup:e$) >> -> *)
(*       let es = Ast.list_of_expr e [] in *)
(*       List.fold_right (fun a b -> <:expr< $a$ + $b$ >>) *)
(* 	es <:expr< 0 >> *)
(* 	(\* FIXME remove 0 *\) *)
(*   | x -> x  *)
(*   ] *)
(* end)#str_item; *)


(**
   learn revised syntax
   camlp4o -printer r -str ' (!+) (1,2,3) '
*)

value foldr_funs = ref [];
value foldl_funs = ref [];

AstFilters.register_str_item_filter (Ast.map_expr begin fun
   [
     (** 1 2 3 4 can also be parsed as an expression *)
    <:expr@_loc< def_foldr $lid:name$ $e$ >> -> begin
      prerr_string (name ^ "detected\n");
     foldr_funs.val := [ (name, e) :: foldr_funs.val ];
      <:expr<()>> (* side-effect only *)
    end 
   | <:expr@_loc< def_foldl $lid:name$ $e$ >> -> begin
     prerr_string (name ^ "detected\n");
     foldl_funs.val := [(name, e) :: foldl_funs.val ];
     <:expr<()>>
   end 
   | e -> e
   ]
end)#str_item;

AstFilters.register_str_item_filter (Ast.map_expr begin fun
   [
     (** how to factor out sub-expression is tricky here you can not
	 just use app, since it's left associativity, not what you want
     *)
     <:expr@_loc< $lid:name$ ($tup:e$) >>
       when List.mem_assoc name !foldl_funs -> begin
	 prerr_string (name ^ "detected!!\n");
	 let op = List.assoc name !foldl_funs in
	 let rec foldl = fun
	   [ [] -> assert False
	   | [x] -> x
	   | [x :: xs] -> <:expr< $op$ $foldl xs$ $x$ >>
	   ]
	 in foldl (List.rev (Ast.list_of_expr e []));
       end 
   | <:expr@_loc< $lid:name$ ($tup:e$) >>
       when List.mem_assoc name !foldr_funs ->
     let op = List.assoc name !foldr_funs in
     let rec foldr = fun
       [ [] -> assert False
       | [x] -> x
       | [x :: xs] -> <:expr@_loc< $op$ $x$ $foldr xs$ >>
       ]
     in foldr (Ast.list_of_expr e [])
   | e -> e]
end)#str_item;

let prl = List.iter (fun [(name,e) -> begin
  print_string name;
(* print_string e; *)
end ]) in  begin
  prl foldl_funs.val;
  prl foldr_funs.val;
end ;
