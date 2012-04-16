value match_case =
  <:match_case<  $list: List.map (fun c -> <:match_case< $uid:c$ -> $`str:c$ >> ) cons $  >>
;
(*
  McOr (
   McArr (
    PaId ( IdUid ( "A")),  ExNil   ExStr ( "A")),
   McOr (
    McArr (
     PaId ( IdUid ( "B")), ExNil   ExStr ( "B")),
    McArr (
     PaId (  IdUid ( "C")), ExNil  ExStr ( "C"))))  
*)

value to_string = <:expr< fun [ $match_case$ ] >>;
(*
  ExFun (
  McOr (
    McArr (
    PaId ( IdUid ( "A")),  ExNil   ExStr ( "A")),
   McOr (
    McArr (
     PaId ( IdUid ( "B")), ExNil   ExStr ( "B")),
    McArr (
     PaId (  IdUid ( "C")), ExNil  ExStr ( "C")))))
*)  

value pim = Printers.OCamlr.print_implem;
pim <:str_item< $exp: to_string $ >>;
(* fun [ A -> "A" | B -> "B" | C -> "C" ]; *)
value match_case2 =
  <:match_case< $list:
  List.map (fun c -> <:match_case< $`str:c$ -> $uid:c$ >> ) 
  cons $ >>;  

pim  <:str_item< $exp: <:expr< fun [$match_case2$ | _ -> invalid_arg "haha"] >> $ >>  ;
(**
   fun [ "A" -> A | "B" -> B | "C" -> C | _ -> invalid_arg "haha" ];
 *)
