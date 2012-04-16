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
value x : ctyp =
  TyOr (TyId (IdUid "A"))
   (TyOr  (TyId  (IdUid  "B"))  (TyId  (IdUid  "C")))
*)   
value  x = <:ctyp< [A | B | C]>>
;
(**
value x : ctyp =
  TySum
   (TyOr  (TyOr
   (TyId   (IdUid  "A"))
   (TyId   (IdUid  "B")))
     (TyId  (IdUid  "C")))
*)
  
<:str_item< type t = [A|B|C]>>;

(**
StTyp 
 (TyDcl  "t" []
   (TySum 
     (TyOr 
       (TyOr 
         (TyId 
           (IdUid  "A"))
         (TyId 
           (IdUid  "B")))
       (TyId 
         (IdUid  "C"))))
   []) *)



