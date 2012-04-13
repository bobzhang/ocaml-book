open Camlp4.PreCast;
open BatPervasives;  

value cons = ["A"; "B";"C"]
and _loc = Loc.ghost ;

value tys =
  <:ctyp< [ $list: (List.map (fun str -> <:ctyp< $uid:str$ >>) cons) $ ]
    >>
;

(** incomplete type *)
value  x = <:ctyp< A | B | C>>
;
(**
value x : Camlp4.PreCast.Ast.ctyp =
  Camlp4.PreCast.Ast.TyOr <abstr>
   (Camlp4.PreCast.Ast.TyId <abstr> (Camlp4.PreCast.Ast.IdUid <abstr> "A"))
   (Camlp4.PreCast.Ast.TyOr <abstr>
     (Camlp4.PreCast.Ast.TyId <abstr> (Camlp4.PreCast.Ast.IdUid <abstr> "B"))
     (Camlp4.PreCast.Ast.TyId <abstr> (Camlp4.PreCast.Ast.IdUid <abstr> "C")))
*)   
value  x = <:ctyp< [A | B | C]>>
;
(**
value x : Camlp4.PreCast.Ast.ctyp =
  Camlp4.PreCast.Ast.TySum <abstr>
   (Camlp4.PreCast.Ast.TyOr <abstr>
     (Camlp4.PreCast.Ast.TyOr <abstr>
       (Camlp4.PreCast.Ast.TyId <abstr>
         (Camlp4.PreCast.Ast.IdUid <abstr> "A"))
       (Camlp4.PreCast.Ast.TyId <abstr>
         (Camlp4.PreCast.Ast.IdUid <abstr> "B")))
     (Camlp4.PreCast.Ast.TyId <abstr> (Camlp4.PreCast.Ast.IdUid <abstr> "C")))
*)
  
<:str_item< type t = [A|B|C]>>;

(**
- : Camlp4.PreCast.Ast.str_item =
Camlp4.PreCast.Ast.StTyp <abstr>
 (Camlp4.PreCast.Ast.TyDcl <abstr> "t" []
   (Camlp4.PreCast.Ast.TySum <abstr>
     (Camlp4.PreCast.Ast.TyOr <abstr>
       (Camlp4.PreCast.Ast.TyOr <abstr>
         (Camlp4.PreCast.Ast.TyId <abstr>
           (Camlp4.PreCast.Ast.IdUid <abstr> "A"))
         (Camlp4.PreCast.Ast.TyId <abstr>
           (Camlp4.PreCast.Ast.IdUid <abstr> "B")))
       (Camlp4.PreCast.Ast.TyId <abstr>
         (Camlp4.PreCast.Ast.IdUid <abstr> "C"))))
   []) *)
  


value match_case =
  <:match_case<  $list: List.map (fun c -> <:match_case< $uid:c$ -> $`str:c$ >> ) cons $  >>
;
(*
  Camlp4.PreCast.Ast.McOr (<abstr>,
   Camlp4.PreCast.Ast.McArr (<abstr>,
    Camlp4.PreCast.Ast.PaId (<abstr>,
     Camlp4.PreCast.Ast.IdUid (<abstr>, "A")),
    Camlp4.PreCast.Ast.ExNil <abstr>,
    Camlp4.PreCast.Ast.ExStr (<abstr>, "A")),
   Camlp4.PreCast.Ast.McOr (<abstr>,
    Camlp4.PreCast.Ast.McArr (<abstr>,
     Camlp4.PreCast.Ast.PaId (<abstr>,
      Camlp4.PreCast.Ast.IdUid (<abstr>, "B")),
     Camlp4.PreCast.Ast.ExNil <abstr>,
     Camlp4.PreCast.Ast.ExStr (<abstr>, "B")),
    Camlp4.PreCast.Ast.McArr (<abstr>,
     Camlp4.PreCast.Ast.PaId (<abstr>,
      Camlp4.PreCast.Ast.IdUid (<abstr>, "C")),
     Camlp4.PreCast.Ast.ExNil <abstr>,
     Camlp4.PreCast.Ast.ExStr (<abstr>, "C"))))  
*)

value to_string = <:expr< fun [ $match_case$ ] >>;
(*
    Camlp4.PreCast.Ast.ExFun (<abstr>,
   Camlp4.PreCast.Ast.McOr (<abstr>,
    Camlp4.PreCast.Ast.McArr (<abstr>,
     Camlp4.PreCast.Ast.PaId (<abstr>,
      Camlp4.PreCast.Ast.IdUid (<abstr>, "A")),
     Camlp4.PreCast.Ast.ExNil <abstr>,
     Camlp4.PreCast.Ast.ExStr (<abstr>, "A")),
    Camlp4.PreCast.Ast.McOr (<abstr>,
     Camlp4.PreCast.Ast.McArr (<abstr>,
      Camlp4.PreCast.Ast.PaId (<abstr>,
       Camlp4.PreCast.Ast.IdUid (<abstr>, "B")),
      Camlp4.PreCast.Ast.ExNil <abstr>,
      Camlp4.PreCast.Ast.ExStr (<abstr>, "B")),
     Camlp4.PreCast.Ast.McArr (<abstr>,
      Camlp4.PreCast.Ast.PaId (<abstr>,
       Camlp4.PreCast.Ast.IdUid (<abstr>, "C")),
      Camlp4.PreCast.Ast.ExNil <abstr>,
      Camlp4.PreCast.Ast.ExStr (<abstr>, "C")))))
*)  

value pim = Printers.OCamlr.print_implem
;

pim
  <:str_item< $exp: to_string $ >>
;
(* fun [ A -> "A" | B -> "B" | C -> "C" ]; *)

value match_case2 =
  <:match_case< $list:
  List.map (fun c -> <:match_case< $`str:c$ -> $uid:c$ >> ) 
  cons $ >>
;  

pim
  <:str_item< $exp: <:expr< fun [$match_case2$ | _ -> invalid_arg "haha"] >> $ >>
  ;
(**
   fun [ "A" -> A | "B" -> B | "C" -> C | _ -> invalid_arg "haha" ];
 *)
