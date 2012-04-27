open Common;
open Util;
open Camlp4.PreCast;

(**
   Compiler Source:
   | "module"; i = a_UIDENT; mb = module_binding0 ->
   <:str_item< module $i$ = $mb$ >>
   | "module"; "rec"; mb = module_binding ->
   <:str_item< module rec $mb$ >>
*)
value scan_modules  module_name (transform: list (string * (Ast.ctyp->Ast.str_item)))
    = object
  inherit Ast.map as super;
  value mutable in_trash_module = False;
  value table = Hashtbl.create 50 ;
  value mutable names = [];
  method! str_item x = match x with 
  [ <:str_item@_loc<
    module $name$ = struct $stri:body$ end >>  when name = module_name -> do {
      prerr_endlinef "module %s found" module_name ;
      let old = in_trash_module ;
      in_trash_module := True;
      super#str_item body |> ignore ; (** for side effects *)
      in_trash_module := old;
      Ast.stSem_of_list
	  (List.map (fun n ->
	    Ast.stSem_of_list (List.rev (Hashtbl.find_all table n)))  names)
    }
  | <:str_item@_loc<
      module $name$ = $_$  >> when name = module_name -> do {
      prerr_endlinef "only module %s = struct ... end supported" name;
      assert False 
   }

  | <:str_item@_loc< type $typ:ty_def$ >> when in_trash_module -> do {
    List.iter (fun (name, trans)
        -> begin
	  p_ctyp_r ty_def;
	  Hashtbl.add table name (trans (super#ctyp ty_def ));
	end 
	 ) transform;
    <:str_item< >>;
    }
  | other -> super#str_item other 
  ];
  initializer begin
    names := List.map fst transform;
  end; 
end;


value trash = ref "Camlp4Trash";
Camlp4.Options.add "-trash" (Arg.Set_string trash);
value register f =
  AstFilters.register_str_item_filter (scan_modules trash.val f)#str_item;


(* AstFilters.register_str_item_filter filter *)
(* ; *)

(* value obj type_decls  = object *)
(*   inherit Ast.map as super; *)
(*   method! str_item str = *)
(*     match str with *)
(*     [ <:str_item@_loc< *)
(*       module $name$ =  $_$   >> -> *)
(*      (\** attention here no semi can put here *)
(* 	 the subtleness lies that it will generate StSem junk *)
(*      *\)	 *)
(* 	if  name = trash.val  then do { *)
(* 	  (\** prerr_endline name; *\) *)
(* 	  match populate.val with *)
(*           [ None -> (populate.val := Some (find_type_decls#str_item str)#get; ) *)
(* 	  | Some _ -> () ] *)
(*             ; *)
(* 	    <:str_item< >> *)
(*             ; *)
(* 	} *)
(*       else do{ *)
(* 	(\** prerr_endline name; *\) *)
(* 	super#str_item str ; *)
(*       } *)
(*     | e -> super#str_item e ] ; *)

(*   method! module_expr me = do{ *)
(*     match  me with *)
(* 	[ <:module_expr@_loc< Camlp4Filters.MetaGeneratorExpr  $id:i$ >> -> *)
(* 	  do{ *)
(* 	    (\* prerr_endline "MetaGeneratorExpr"; *\) *)
(* 	    <:module_expr< *)
(* 	      struct *)
(* 		$Template.expr_basic_template _loc $; *)
(* 		open $i$; *)
(* 		value rec $match populate.val with  *)
(* 		       [Some map -> gen_expr_binding map *)
(* 		       |None -> do{ *)
(* 			 prerr_endlinef  "No Trash module %s defined " trash.val; *)
(* 			 assert False; *)
(* 		       } *)
(* 		       ] $; *)
(* 	      end  *)
(*             >>; *)
(* 	  } *)
(* 	| <:module_expr@_loc< Camlp4Filters.MetaGeneratorPatt $id:i$ >> -> *)
(* 	  do{ *)
(* 	    (\* prerr_endline "MetaGeneratorPatt"; *\) *)
(* 	    <:module_expr< *)
(* 	      struct *)
(* 		$Template.patt_basic_template _loc $; *)
(* 		open $i$; *)
(* 		value rec $match populate.val with *)
(* 		       [Some map -> gen_patt_binding map *)
(* 		       |None -> do { *)
(* 			 prerr_endlinef "No Trash module %s defined " trash.val; *)
(* 			 assert False; *)
(* 		       }] $ ; *)
(* 	      end  *)
(* 	    >>; *)
(* 	  } *)
(* 	| e -> super#module_expr e ]; *)
(*   }; *)

(*  end *)
(* ; *)


(*  value filter st = *)
(*     (\** lazy evaluation for the purpuse of just scanning once *\) *)
(*     let type_decls = lazy (find_type_decls#str_item st)#get in *)
(*     do{ *)
(*       (\* prerr_endline "(\\** "; *\) *)
(*       (\* p_str_item st; *\) *)
(*       (\* prerr_endline "  *\\)"; *\) *)
(*       (obj type_decls)#str_item st; *)
(*     } *)
(* ; *)
(* (\* prerr_endline "Test"; *\) *)
