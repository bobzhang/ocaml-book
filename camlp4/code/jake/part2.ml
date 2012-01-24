(* #require "camlp4.gramlib";; *)
open Camlp4.PreCast
open BatPervasives  
let cons = ["A"; "B";"C"] and _loc = Loc.ghost ;;
let tys = Ast.tyOr_of_list
  (List.map (fun str -> <:ctyp< $uid:str$ >>) cons);;

(*
val tys : Camlp4.PreCast.Ast.ctyp =
  Camlp4.PreCast.Ast.TyOr (<abstr>,
   Camlp4.PreCast.Ast.TyId (<abstr>, Camlp4.PreCast.Ast.IdUid (<abstr>, "A")),
   Camlp4.PreCast.Ast.TyOr (<abstr>,
    Camlp4.PreCast.Ast.TyId (<abstr>,
     Camlp4.PreCast.Ast.IdUid (<abstr>, "B")),
    Camlp4.PreCast.Ast.TyId (<abstr>,
     Camlp4.PreCast.Ast.IdUid (<abstr>, "C"))))
*)
(** here you can better understand what ctyp really means, a type
expression, not a top-level struct, cool 
*)
let verify = <:ctyp< A |B |C>>;;

let _ = begin
  print_bool (verify = tys);
end 

(*
  true
*)  

let type_def = <:str_item< type t = $tys$ >>
(*
val type_def : Camlp4.PreCast.Ast.str_item =
  Camlp4.PreCast.Ast.StTyp (<abstr>,
   Camlp4.PreCast.Ast.TyDcl (<abstr>, "t", [],
    Camlp4.PreCast.Ast.TyOr (<abstr>,
     Camlp4.PreCast.Ast.TyId (<abstr>,
      Camlp4.PreCast.Ast.IdUid (<abstr>, "A")),
     Camlp4.PreCast.Ast.TyOr (<abstr>,
      Camlp4.PreCast.Ast.TyId (<abstr>,
       Camlp4.PreCast.Ast.IdUid (<abstr>, "B")),
      Camlp4.PreCast.Ast.TyId (<abstr>,
       Camlp4.PreCast.Ast.IdUid (<abstr>, "C")))),
    []))
*)

let _ =  begin
  Printers.OCaml.print_implem type_def ;
end
(*
  type t = A | B | C
*)  


(** always ambiguous when manipulating ast using original syntax
    recommend using revised syntx
*)  
let verify2 = <:str_item< type t =  [ A | B | C ] >>;;
(*
val verify2 : Camlp4.PreCast.Ast.str_item =
  Camlp4.PreCast.Ast.StTyp (<abstr>,
   Camlp4.PreCast.Ast.TyDcl (<abstr>, "t", [],
    Camlp4.PreCast.Ast.TySum (<abstr>,
     Camlp4.PreCast.Ast.TyOr (<abstr>,
      Camlp4.PreCast.Ast.TyOr (<abstr>,
       Camlp4.PreCast.Ast.TyId (<abstr>,
        Camlp4.PreCast.Ast.IdUid (<abstr>, "A")),
       Camlp4.PreCast.Ast.TyId (<abstr>,
        Camlp4.PreCast.Ast.IdUid (<abstr>, "B"))),
      Camlp4.PreCast.Ast.TyId (<abstr>,
       Camlp4.PreCast.Ast.IdUid (<abstr>, "C")))),
    []))
*)
let _ = begin
  print_bool (verify2 = type_def);
  Printers.OCaml.print_implem verify2
end

(*
  false
  type t = | A | B | C;;
*)  

let match_case =
  List.map 
    (fun c -> <:match_case< $uid:c$ -> $`str:c$ >>) cons
    |> Ast.mcOr_of_list ;;
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
let to_string = <:expr< fun [ $match_case$ ] >>;;
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

let pim = Printers.OCaml.print_implem

let _ = begin
  pim <:str_item< let a = $to_string$ in a >>;
end 
(*
  let a = function | A -> "A" | B -> "B" | C -> "C" in a;;  
*)

let match_case2 = List.map
  (fun c -> <:match_case< $`str:c$ -> $uid:c$
    >>) cons|> Ast.mcOr_of_list 


let _ = begin 
  pim  <:str_item<let f = fun [ $match_case2$ ] in f  >>;
  pim <:str_item<let f = fun [ $match_case2$ | _ -> invalid_arg "haha" ] in f >>;
end 

(*

let f = function | "A" -> A | "B" -> B | "C" -> C in f;;
let f = function | "A" -> A | "B" -> B | "C" -> C | _ -> invalid_arg "haha"
in f;;
  
*)
      
