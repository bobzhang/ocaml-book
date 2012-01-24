value antiquot_expander = object
  inherit Ast.map as super;
  method patt = fun
      [ <:patt@_loc< $anti:s$ >> | <:patt@_loc< $str:s$ >> as p ->
          let mloc _loc = MetaLoc.meta_loc_patt _loc _loc in
          handle_antiquot_in_string s p TheAntiquotSyntax.parse_patt _loc (fun n p ->
            match n with
            [ "antisig_item" -> <:patt< Ast.SgAnt $mloc _loc$ $p$ >>
            | "antistr_item" -> <:patt< Ast.StAnt $mloc _loc$ $p$ >>
            | "antictyp" -> <:patt< Ast.TyAnt $mloc _loc$ $p$ >>
            | "antipatt" -> <:patt< Ast.PaAnt $mloc _loc$ $p$ >>
            | "antiexpr" -> <:patt< Ast.ExAnt $mloc _loc$ $p$ >>
            | "antimodule_type" -> <:patt< Ast.MtAnt $mloc _loc$ $p$ >>
            | "antimodule_expr" -> <:patt< Ast.MeAnt $mloc _loc$ $p$ >>
            | "anticlass_type" -> <:patt< Ast.CtAnt $mloc _loc$ $p$ >>
            | "anticlass_expr" -> <:patt< Ast.CeAnt $mloc _loc$ $p$ >>
            | "anticlass_sig_item" -> <:patt< Ast.CgAnt $mloc _loc$ $p$ >>
            | "anticlass_str_item" -> <:patt< Ast.CrAnt $mloc _loc$ $p$ >>
            | "antiwith_constr" -> <:patt< Ast.WcAnt $mloc _loc$ $p$ >>
            | "antibinding" -> <:patt< Ast.BiAnt $mloc _loc$ $p$ >>
            | "antirec_binding" -> <:patt< Ast.RbAnt $mloc _loc$ $p$ >>
            | "antimatch_case" -> <:patt< Ast.McAnt $mloc _loc$ $p$ >>
            | "antimodule_binding" -> <:patt< Ast.MbAnt $mloc _loc$ $p$ >>
            | "antiident" -> <:patt< Ast.IdAnt $mloc _loc$ $p$ >>
            | _ -> p ])
            | p -> super#patt p ];
    method expr = fun
      [ <:expr@_loc< $anti:s$ >> | <:expr@_loc< $str:s$ >> as e ->
          let mloc _loc = MetaLoc.meta_loc_expr _loc _loc in
          handle_antiquot_in_string s e TheAntiquotSyntax.parse_expr _loc (fun n e ->
            match n with
            [ "`int" -> <:expr< string_of_int $e$ >>
            | "`int32" -> <:expr< Int32.to_string $e$ >>
            | "`int64" -> <:expr< Int64.to_string $e$ >>
            | "`nativeint" -> <:expr< Nativeint.to_string $e$ >>
            | "`flo" -> <:expr< Camlp4_import.Oprint.float_repres $e$ >>
            | "`str" -> <:expr< Ast.safe_string_escaped $e$ >>
            | "`chr" -> <:expr< Char.escaped $e$ >>
            | "`bool" -> <:expr< Ast.IdUid $mloc _loc$ (if $e$ then "True" else "False") >>
            | "liststr_item" -> <:expr< Ast.stSem_of_list $e$ >>
            | "listsig_item" -> <:expr< Ast.sgSem_of_list $e$ >>
            | "listclass_sig_item" -> <:expr< Ast.cgSem_of_list $e$ >>
            | "listclass_str_item" -> <:expr< Ast.crSem_of_list $e$ >>
            | "listmodule_expr" -> <:expr< Ast.meApp_of_list $e$ >>
            | "listmodule_type" -> <:expr< Ast.mtApp_of_list $e$ >>
            | "listmodule_binding" -> <:expr< Ast.mbAnd_of_list $e$ >>
            | "listbinding" -> <:expr< Ast.biAnd_of_list $e$ >>
            | "listbinding;" -> <:expr< Ast.biSem_of_list $e$ >>
            | "listrec_binding" -> <:expr< Ast.rbSem_of_list $e$ >>
            | "listclass_type" -> <:expr< Ast.ctAnd_of_list $e$ >>
            | "listclass_expr" -> <:expr< Ast.ceAnd_of_list $e$ >>
            | "listident" -> <:expr< Ast.idAcc_of_list $e$ >>
            | "listctypand" -> <:expr< Ast.tyAnd_of_list $e$ >>
            | "listctyp;" -> <:expr< Ast.tySem_of_list $e$ >>
            | "listctyp*" -> <:expr< Ast.tySta_of_list $e$ >>
            | "listctyp|" -> <:expr< Ast.tyOr_of_list $e$ >>
            | "listctyp," -> <:expr< Ast.tyCom_of_list $e$ >>
            | "listctyp&" -> <:expr< Ast.tyAmp_of_list $e$ >>
            | "listwith_constr" -> <:expr< Ast.wcAnd_of_list $e$ >>
            (* interesting bits *)
            | "listmatch_case" -> <:expr< Ast.mcOr_of_list $e$ >> 
            | "listpatt," -> <:expr< Ast.paCom_of_list $e$ >>
            | "listpatt;" -> <:expr< Ast.paSem_of_list $e$ >>
            | "listexpr," -> <:expr< Ast.exCom_of_list $e$ >>
            | "listexpr;" -> <:expr< Ast.exSem_of_list $e$ >>
            | "antisig_item" -> <:expr< Ast.SgAnt $mloc _loc$ $e$ >>
            | "antistr_item" -> <:expr< Ast.StAnt $mloc _loc$ $e$ >>
            | "antictyp" -> <:expr< Ast.TyAnt $mloc _loc$ $e$ >>
            | "antipatt" -> <:expr< Ast.PaAnt $mloc _loc$ $e$ >>
            | "antiexpr" -> <:expr< Ast.ExAnt $mloc _loc$ $e$ >>
            | "antimodule_type" -> <:expr< Ast.MtAnt $mloc _loc$ $e$ >>
            | "antimodule_expr" -> <:expr< Ast.MeAnt $mloc _loc$ $e$ >>
            | "anticlass_type" -> <:expr< Ast.CtAnt $mloc _loc$ $e$ >>
            | "anticlass_expr" -> <:expr< Ast.CeAnt $mloc _loc$ $e$ >>
            | "anticlass_sig_item" -> <:expr< Ast.CgAnt $mloc _loc$ $e$ >>
            | "anticlass_str_item" -> <:expr< Ast.CrAnt $mloc _loc$ $e$ >>
            | "antiwith_constr" -> <:expr< Ast.WcAnt $mloc _loc$ $e$ >>
            | "antibinding" -> <:expr< Ast.BiAnt $mloc _loc$ $e$ >>
            | "antirec_binding" -> <:expr< Ast.RbAnt $mloc _loc$ $e$ >>
            | "antimatch_case" -> <:expr< Ast.McAnt $mloc _loc$ $e$ >>
            | "antimodule_binding" -> <:expr< Ast.MbAnt $mloc _loc$ $e$ >>
            | "antiident" -> <:expr< Ast.IdAnt $mloc _loc$ $e$ >>
            | _ -> e ])
      | e -> super#expr e ];
