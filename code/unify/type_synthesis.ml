(* Author: bobzhang1988@vpl729.wireless-pennnet.upenn.edu        *)
(* Version: $Id: type_synthesis.ml,v 0.0 2012/02/16 15:59:24 bobzhang1988 Exp $ *)
open Printf

open Util
open Unify
open Mlast

let var (n:int)  =
  Var("v"^string_of_int n)

let const c =
  Term (c,[])

let pair(t1,t2)=Term("pair",[t1;t2])

let arrow(t1,t2) = Term("arrow",[t1;t2])

let new_int,reset_new_int =
  let c = ref (-1) in
  (fun () -> incr c; !c),
  (fun () -> c := -1 )

let unop_type k = match k with
  | Ml_fst ->
    let a = var (new_int())
    and b = var (new_int()) in
    pair(a,b),a

  | Ml_snd ->
    let a = var (new_int())
    and b = var (new_int()) in
    pair(a,b),b

let binop_type k = match k with
  | Ml_add |Ml_sub | Ml_mult
    ->
    (const "int", const "int", const "int")
  | Ml_eq | Ml_gt | Ml_less ->
    (const "int", const"int", const "bool")


let generate_type_constraints e = begin
  reset_new_int();
  (** n is pointed to current expr *)
  let rec gen n tenv = function
    | <:me< $int:_$ >> -> [var n, const "int"]
    | <:me< $bool:_$ >> -> [var n, const "bool"]
    | <:me< fst $_$ >> ->
      let t1,t2 = unop_type Ml_fst
      and ne  = new_int () in
      [(var n, t2) ; (var ne, t1)]
    | <:me< snd $_$ >> ->
      let t1,t2 = unop_type Ml_snd
      and ne = new_int () in
      [(var n, t2) ; (var ne, t1)]
	(** consider to improve infixop support later*)
    | <:me< $e1$ + $e2$ >> 
    | <:me< $e1$ - $e2$ >>
    | <:me< $e1$ * $e2$ >> -> 
      let n1 = new_int()
      and n2 = new_int() in 
      (var n, const "int") ::
	(var n1, const "int") :: (var n2, const "int") ::
	(gen n1 tenv e1) @ (gen n2 tenv e2)
    | <:me< $e1$ = $e2$ >>
    | <:me< $e1$ < $e2$ >>
    | <:me< $e1$ > $e2$ >> -> 
      let n1 = new_int () and n2 = new_int () in
      (var n, const "bool") :: (var n1, const "int")
	:: (var n2, const "int") ::
	(gen n1 tenv e1) @ (gen n2 tenv e2)
    | <:me< ( $e1$, $e2$ ) >>  ->
      let n1 = new_int () and  n2 = new_int () in
      (var n, pair (var n1, var n2)) ::
	(gen n1 tenv e1 @ gen n2 tenv e2)
    | <:me< $lid:x$ >> ->
      [var n , List.assoc x tenv ]
    | <:me< if $e1$ then $e2$ else $e3$ >> -> 
      let n1 = new_int () and n2 = new_int () and n3 = new_int () in
      (var n , var n2):: (var n2, var n3) :: (var n1 , const "bool")
	:: (gen n1 tenv e1) @ (gen n2 tenv e2) @ (gen n3 tenv e3)
    | <:me< fun $patt:x$ -> $e$ >> -> 
      let n1 = new_int () and n2 = new_int () in
      (var n, arrow(var n1, var n2) ) ::
	(gen n2  ((x, var n1) :: tenv ) e)

    | <:me< $e1$ $e2$ >> ->
      let n1 = new_int () and n2 = new_int() in
      (var n1 , arrow(var n2, var n)) ::
	(gen n1 tenv e1) @ (gen n2 tenv e2)

    | <:me< let $patt:x$ = $e1$ in $e2$ >> ->
      let n1 = new_int () and n2 = new_int () in
      (var n , var n2 ) ::
	(gen n1 tenv e1)@
	(gen n2 ((x,var n1) :: tenv ) e2)

    | <:me< let rec $patt:x$ = $e1$ in $e2$ >> ->
      let n1 = new_int () and n2 = new_int () in
      (var n, var n2) ::
	(gen n1 ((x, var n1)::tenv) e1) @
	(gen n2 ((x,var n1)::tenv) e2)

    (** for safety *)	
    | Ml_Ant (_,_)
    | Ml_fun (Ml_patAnt (_,_),_ )
    | Ml_let (Ml_patAnt (_,_),_ ,_ )
    | Ml_letrec (Ml_patAnt (_,_),_ ,_ )-> failwithf "not supported ast "
  in  gen (new_int ()) [] e   
end 

  
    
    












