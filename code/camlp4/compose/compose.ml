open Camlp4.PreCast;
value gen _loc patts exprs =
    <:match_case<
      $list: List.map2 (fun p e -> <:match_case< $p$ -> $e$ >> )
      patts exprs $ >> ;
(**
let gen _loc patts exprs =
  Ast.mcOr_of_list
    (List.map2 (fun p e -> Ast.McArr (_loc, p, (Ast.ExNil _loc), e)) patts
       exprs)
*)


(** a tedious way *)
value gen2 patts exprs =
    let cases = List.fold_right2 begin fun p e acc ->
      let _loc = Loc.merge (Ast.loc_of_patt p) (Ast.loc_of_expr e) in
      <:match_case< $p$ -> $e$ | $acc$ >>
    end patts exprs <:match_case@here< >>
      (** here is handled specifically
	  (Loc.of_tuple ("ghost-location", 1, 0, 0, 1, 0, 0, true))
      *)
    in
    let _loc = Ast.loc_of_match_case cases in
    <:expr< fun [ $cases$ ] >>
    ;

value data_con_args _loc n t =
	let rec self n =
	  if n <= 0 then <:ctyp< >>
	  else <:ctyp<  $t$  and $self (n-1)$ >> (** minor location error ??? *)
	in
	self n ;

value data_con _loc n t =
  <:ctyp< $uid:"C" ^ string_of_int n $ of $data_con_args _loc n t $ >>
;

value gen_type _loc n t =
  let rec self n = 
    if n <= 0
    then <:ctyp< >>
    else <:ctyp<   $self (n-1)$ | $data_con _loc n t $  >>
  (* <:ctyp<   [ $self (n-1)$ | $data_con _loc n t $ ] >> *)
  in
  <:ctyp< [ $ self n $ ] >>
  (* <:ctyp<  $ self n $  >>
     subtle difference try and you will know why
  *)
;

value filter = fun
  [ <:ctyp@_loc< gen_type $lid:x$ >>
  | <:ctyp@_loc< $lid:x$ gen_type >> ->
    (** Scanf.sscanf *)
    Scanf.sscanf x "%[^0-9]%d" begin fun _ n -> begin 
      (* prerr_endline (string_of_int n); *)
      gen_type _loc n  <:ctyp< $lid:x$ >>
    end 
   end
  | t -> t 
  ];
	
AstFilters.register_str_item_filter (Ast.map_ctyp filter )#str_item;  

(**
   ocamlc -c -pp 'camlp4o -I _build compose.cmo'  test.ml
   camlp4o -I _build compose.cmo -str 'type t7 = gen_type t7'
7
type t7 =
  | C1 of t7
  | C2 of t7 * t7
  | C3 of t7 * t7 * t7
  | C4 of t7 * t7 * t7 * t7
  | C5 of t7 * t7 * t7 * t7 * t7
  | C6 of t7 * t7 * t7 * t7 * t7 * t7
  | C7 of t7 * t7 * t7 * t7 * t7 * t7 * t7
*)


