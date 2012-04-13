open Common;
open Util;
open Camlp4.PreCast;
open Ppo; (** for printing and parsing *)

value use_loc = ref True ;

(* begin *)
(*     "Trash  for your defined type and will be removed default: Camlp4Trash"; *)
(*   Camlp4.Options.add "-use_loc" (Arg.Set use_loc ) *)
(* end *)
(* ; *)

(**
   given a type b, generate an ast of a function transfrom the value of type b
   to its ast representation
 *)


value f_expr_of_ctyp ty =
    let _loc = Ast.loc_of_ctyp ty in 
    f_of_ctyp ty (<:expr< Ast.ExTup _loc >>, <:expr<Ast.ExCom _loc >> );


(**
f_patt_of_ctyp <:ctyp< (int * (int * bool) * bool) >> |> p_expr; 
fun _loc (x0, x1, x2) ->
  Ast.PaTup _loc
    (Ast.PaCom _loc (meta_int _loc x0)
       ((fun _loc (x0, x1) ->
           Ast.PaTup _loc
             (Ast.PaCom _loc (meta_int _loc x0) (meta_bool _loc x1)))
          _loc x1)
       (meta_bool _loc x2))
   Given a type, generate an ast of a function which generates the representation of the ast
   to represent the value 
 *)    
value f_patt_of_ctyp ty =
    let _loc = Ast.loc_of_ctyp ty in 
    f_of_ctyp ty (<:expr< Ast.PaTup _loc >>, <:expr<Ast.PaCom _loc  >>);

    
value built_in_simple_ant =
  ["BAnt"; "OAnt"; "LAnt"; "ReAnt"; "DiAnt";
   "MuAnt"; "PrAnt"; "ViAnt"; "OvAnt"; "RvAnt"]
;

value is_loc = fun
  [ <:ctyp< $_$ of $_$ and Loc.t >>
  | <:ctyp< $_$ of Loc.t >>  -> True
  | _ -> False ];

(** pattern match makes check the last type easier than the first *)      
value check_loc (ty: Ast.ctyp) =
  let lst = (Ast.list_of_ctyp ty []) in
  match lst with
  [ [] -> `NoLoc
  | [h::t] ->
      if is_loc h then
	if List.for_all is_loc t then
	  `Loc
	else
	  `Inconsistent
      else
	if List.for_all (fun x -> not (is_loc x)) t then
	  `NoLoc
	else `Inconsistent
  ];

	


(** mainly handle all branches of ctors *)      
value gen_match_case
      (ty:Ast.ctyp)
      (of_id,app,of_ctyp)
      frag :Ast.match_case =
    let _loc = Loc.ghost in     
    fold_data_ctors ty (fun (cons:string) (tyargs : list Ast.ctyp) acc ->

      let con_id = <:ident< $uid:cons$ >> in
      let len = List.length tyargs in 
      let p = gen_patt len "x" <:patt< $uid:cons$ >> _loc in 
      let e =
	if is_antiquot_data_ctor cons then
	  gen_expr  len "x" frag  _loc 
	else
	  fold_args_i tyargs (fun ty i acc ->
	    app acc
	      (** we allowed use_loc on but without Loc.t field here *)
	      (if !use_loc then
		  <:expr< $of_ctyp ty$  $id:x i _loc $ >>
		else
		  <:expr< $of_ctyp ty$ _loc $id:x i _loc $ >>))
	      (of_id con_id)  in
	<:match_case< $p$ -> $e$ | $acc$ >>
    ) <:match_case<>>
;



    
(**
   gen_expr_match_case <:ctyp< A of (int option) and bool | B of bool  >> |> p_match_case;
   [ B x0 ->
    Ast.ExApp _loc (Ast.ExId _loc (Ast.IdUid _loc "B")) (meta_bool _loc x0)
   | A x0 x1 ->
    Ast.ExApp _loc
      (Ast.ExApp _loc (Ast.ExId _loc (Ast.IdUid _loc "A"))
         (meta_int meta_option _loc x0))
      (meta_bool _loc x1) ]
   gen_expr_match_case <:ctyp< AAnt of (int option) and bool | B of bool  >> |> p_match_case;
   [ B x0 ->
   Ast.ExApp _loc (Ast.ExId _loc (Ast.IdUid _loc "B")) (meta_bool _loc x0)
   | AAnt x0 x1 -> Ast.ExAnt x0 x1 ]   
 *)    
value gen_expr_match_case ty =
    gen_match_case ty (me_of_id,me_app,f_expr_of_ctyp)
       <:expr< Ast.ExAnt _loc>> ;

value gen_patt_match_case ty =
    gen_match_case ty (mp_of_id,mp_app,f_patt_of_ctyp)
      <:expr< Ast.PaAnt _loc>>;
    

(**
   Given a StringMap of (string,ctyp) generate a recursve binding
   mainly handle type variables
*)
value gen_binding  (m:StringMap.t Ast.ctyp) gen_match_case : Ast.binding =
    StringMap.fold (fun name tydcl acc ->
      match tydcl with
      [ Ast.TyDcl _ _ tyvars <:ctyp< [$ty$ ] >> _ ->
	let match_case = gen_match_case ty in
	let funct =
	  List.fold_right (fun tyvar acc ->
	    match tyvar with
	    [ <:ctyp< + '$s$>> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> ->
	      <:expr< fun $lid:mf_ s $ -> $acc$ >>
	    | _ -> do {
	      p_ctyp tyvar ;
	      assert False;
	    }
	    ]	
	  ) tyvars
	    (if !use_loc then
	      <:expr< fun [$match_case$ ] >>
	    else <:expr< fun _loc -> fun [ $match_case$ ] >> ) in
	<:binding< $acc$ and $lid:"meta_"^name$ = $funct$ >>
    | Ast.TyDcl _ _ _ _ _ -> (** ignore *)
      acc
    | _ -> do{
      p_ctyp tydcl;
      assert False;
    }
    ]
    ) m <:binding< >>
;

value gen_expr_binding m = gen_binding m gen_expr_match_case;
value gen_patt_binding m = gen_binding m gen_patt_match_case;
    














(* (\** *)
(*    given a branch generate the right branch of match_case *)
(*    f_expr_of_ctyp <:ctyp< (int *  bool)  >> |> p_expr; *)
(*    fun _loc (x1, x2) -> *)
(*        Ast.ExTup _loc (Ast.ExCom _loc (meta_int _loc x1) (meta_bool _loc x2)) *)
(* *\) *)
(* value rec f_expr_of_ctyp (ty:Ast.ctyp) :Ast.expr= *)
(*   match ty with *)
(*   [ <:ctyp< $id:id$ >> -> *)
(*     <:expr< $id:meta_ (string_of_ident id) $ >> *)
(*     (\* ty -> meta_ty *\)   *)
(*   | <:ctyp< ( $t1$ * $t2$) >> -> *)
(*     <:expr< fun _loc (x1,x2) -> *)
(*       Ast.ExTup _loc *)
(* 	(Ast.ExCom _loc *)
(* 	   ($f_expr_of_ctyp t1$ _loc x1) *)
(* 	   ($f_expr_of_ctyp t2$ _loc x2)) *)
(*       >> *)
(*   | <:ctyp< $t1$ $t2$ >> ->  *)
(*       <:expr< $f_expr_of_ctyp t1$ $f_expr_of_ctyp t2$ >> *)
(*   | <:ctyp< '$s$ >> -> <:expr< $lid:mf_ s$ >>  *)
(*   | _ -> failure  *)
(*   ] *)
(* ; *)

(* (\** *)
(*    f_patt_of_ctyp <:ctyp< (int *  bool)  >> |> p_expr; *)
(*    fun _loc (x1, x2) -> *)
(*    Ast.PaTup _loc (Ast.PaCom _loc (meta_int _loc x1) (meta_bool _loc x2)) *)
(*    just handle (int*int), int option, *)
   
(*    types like (int * int * int) are not handled  *)
(*  *\) *)
(* value rec f_patt_of_ctyp ty = *)
(*   match ty with *)
(*   [ <:ctyp< $id:id$ >> -> *)
(*     <:expr< $id:meta_ (string_of_ident id) $ >> *)
(*     (\* ty -> meta_ty *\)   *)
(*   | <:ctyp< ( $t1$ * $t2$) >> -> *)
(*     <:expr< fun _loc (x1,x2) -> *)
(*       Ast.PaTup _loc *)
(* 	(Ast.PaCom _loc *)
(* 	   ($f_patt_of_ctyp t1$ _loc x1) *)
(* 	   ($f_patt_of_ctyp t2$ _loc x2)) *)
(*       >> *)
(*   | <:ctyp< $t1$ $t2$ >> ->  *)
(*       <:expr< $f_patt_of_ctyp t1$ $f_patt_of_ctyp t2$ >> *)
(*   | <:ctyp< '$s$ >> -> <:expr< $lid:mf_ s$ >>  *)
(*   | _ -> failure  *)
(*   ] *)
(* ; *)
(* value gen_expr_match_case (ty:Ast.ctyp) : Ast.match_case = *)
(*     fold_data_ctors ty (fun (cons:string) tyargs acc -> *)
(*       let con_id = <:ident< $uid:cons$ >> in *)
(*       let arity = List.length tyargs in  *)
(*       let init = *)
(* 	con_id 	|> me_of_id in *)
(*       let p = patt_of_data_ctor_decl con_id arity in *)
(*       let e = *)
(* 	if List.mem cons built_in_simple_ant *)
(* 	(\* << $id:<:ident< xxxx>> >> ~  << xxxx >>  *\) *)
(* 	then <:expr< Ast.ExAnt _loc x0 >> *)
(* 	else if is_antiquot_data_ctor cons *)
(* 	then expr_of_data_ctor_decl <:ident< Ast.ExAnt >>  arity *)
(* 	else *)
(* 	  fold_args_i tyargs (fun ty i acc -> *)
(* 	    me_app acc <:expr< $f_expr_of_ctyp ty$ _loc $id:x i$ >>  *)
(* 	  ) init in *)
(*       <:match_case< $p$ -> $e$ | $acc$ >> *)
(*     ) <:match_case<>>  *)
(* ; *)

(* (\** *)
   
(*    gen_patt_match_case <:ctyp< AAnt of (int option) and bool | B of bool  >> |> p_match_case; *)
   
(*    [ B x0 -> *)
(*        Ast.PaApp _loc (Ast.PaId _loc (Ast.IdUid _loc "B")) (meta_bool _loc x0) *)
(*    | AAnt x0 x1 -> Ast.PaAnt x0 x1 ] *)
(*  *\) *)
(* value gen_patt_match_case (ty:Ast.ctyp) : Ast.match_case = *)
(*     fold_data_ctors ty (fun (cons:string) tyargs acc -> *)
(*       let con_id = <:ident< $uid:cons$ >> in *)
(*       let arity = List.length tyargs in  *)
(*       let init = *)
(* 	con_id 	|> mp_of_id in (\** patt Fix *\) *)
(*       let p = patt_of_data_ctor_decl con_id arity in *)
(*       let e = *)
(* 	if List.mem cons built_in_simple_ant *)
(* 	(\* << $id:<:ident< xxxx>> >> ~  << xxxx >>  *\) *)
(* 	then <:expr< Ast.PaAnt _loc x0 >> *)
(* 	else if is_antiquot_data_ctor cons *)
(* 	then expr_of_data_ctor_decl <:ident<Ast.PaAnt >>  arity *)
(* 	else *)
(* 	  fold_args_i tyargs (fun ty i acc -> *)
(* 	    mp_app acc <:expr< $f_patt_of_ctyp ty$ _loc $id:x i$ >> *)
(* 	  ) init in *)
(*       <:match_case< $p$ -> $e$ | $acc$ >> *)
(*     ) <:match_case<>>  *)
(* ; *)
(* (\** Given a StringMap of (string,ctyp) generate a recursve binding *)
(* *\) *)
(* value gen_expr_binding  (m:StringMap.t Ast.ctyp) : Ast.binding = *)
(*     StringMap.fold (fun name tydcl acc -> *)
(*       match tydcl with *)
(*       [ Ast.TyDcl _ _ tyvars <:ctyp< [$ty$ ] >> _ -> *)
(* 	let match_case = gen_expr_match_case ty in *)
(* 	let funct = *)
(* 	  List.fold_right (fun tyvar acc -> *)
(* 	    match tyvar with *)
(* 	    [ <:ctyp< + '$s$>> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> -> *)
(* 	      <:expr< fun $lid:mf_ s $ -> $acc$ >> *)
(* 	    | _ -> do { *)
(* 	      p_ctyp tyvar ; *)
(* 	      assert False; *)
(* 	    } *)
(* 	    ]	 *)
(* 	  ) tyvars <:expr< fun _loc -> fun [ $match_case$ ] >> in *)
(* 	<:binding< $acc$ and $lid:"meta_"^name$ = $funct$ >> *)
(*     | Ast.TyDcl _ _ _ _ _ -> *)
(*       acc *)
(*     | _ -> do{ *)
(*       p_ctyp tydcl; *)
(*       assert False; *)
(*     } *)
(*     ] *)
(*     ) m <:binding< >> *)
(* ; *)


(* value gen_patt_binding  (m:StringMap.t Ast.ctyp) : Ast.binding = *)
(*     StringMap.fold (fun name tydcl acc -> *)
(*       match tydcl with *)
(*       [ Ast.TyDcl _ _ tyvars <:ctyp< [$ty$ ] >> _ -> *)
(* 	let match_case = gen_patt_match_case ty in *)
(* 	let funct = *)
(* 	  List.fold_right (fun tyvar acc -> *)
(* 	    match tyvar with *)
(* 	    [ <:ctyp< + '$s$>> | <:ctyp< -'$s$ >> | <:ctyp< '$s$ >> -> *)
(* 	      <:expr< fun $lid:mf_ s $ -> $acc$ >> *)
(* 	    | _ -> do { *)
(* 	      p_ctyp tyvar ; *)
(* 	      assert False; *)
(* 	    } *)
(* 	    ]	 *)
(* 	  ) tyvars <:expr< fun _loc -> fun [ $match_case$ ] >> in *)
(* 	<:binding< $acc$ and $lid:"meta_"^name$ = $funct$ >> *)
(*     | Ast.TyDcl _ _ _ _ _ -> *)
(*       acc *)
(*     | _ -> do{ *)
(*       p_ctyp tydcl; *)
(*       assert False; *)
(*     } *)
(*     ] *)
(*     ) m <:binding< >> *)
(* ; *)
	(* if List.mem cons built_in_simple_ant *)
	(* (\* << $id:<:ident< xxxx>> >> ~  << xxxx >>  *\) *)
	(* then frag1  *)
	(* else *)
    (* let localize = *)
    (*   match check_loc ty with *)
    (*   [`Inconsistent -> begin *)
    (* 	prerr_endlinef "Incosistent location in:"; *)
    (* 	p_ctyp ty ; *)
    (* 	assert False *)
    (* 	end *)
    (*   | `Loc -> *)
    (* 	  if !use_loc then *)
    (* 	    True *)
    (* 	  else begin *)
    (* 	    prerr_endlinef "You use location but does not turned on -use_loc"; *)
    (* 	    assert False; *)
    (* 	  end  *)
    (*   | `NoLoc -> False  *)
    (*   ] in  *)
