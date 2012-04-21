(* -*- Mode:Tuareg; -*-                                                      
   Version: $Id: common.ml,v 0.0 2012/02/19 01:49:20 bobzhang1988 Exp $ *)

open Camlp4.PreCast;
open Ppo;
open Util;

value x ?(_loc=Loc.ghost) (i:int) : Ast.ident  =
  <:ident< $lid:"x" ^ string_of_int i $ >>;

value meta_ ?(_loc=Loc.ghost)
  (s:string)   : Ast.ident = <:ident< $lid: "meta_" ^ s $ >>;

value mf_ (s:string) = "mf_" ^ s;

(**
   string_list_of_ident <:ident< A.B.x>>;;
- : string list = ["A"; "B"; "x"]
 *)
value rec string_list_of_ident  = fun
  [ <:ident< $lid:s$ >> -> [s]
  | <:ident< $uid:s$ >> -> [s]
  | <:ident< $i1$ . $i2$ >> ->
    string_list_of_ident i1 @ string_list_of_ident  i2
  | ( <:ident< $_$ $_$ >> as x )
  | ( <:ident< $anti:_$ >> as x )  -> do{
    prerr_endlinef "invalid argument of string ident: ";
    p_ident_r x;
    assert False;
  } ] ;

(* string_of_ident "meta_" <:ident< A.B.C.t >>; 
   - : string = "A.B.C.meta_t" 
value string_of_ident (hole:string) (ident:Ast.ident) =
  match List.rev (string_list_of_ident ident) with
    [ [] -> assert False
    | [h::t] -> List.(fold_right (fun x acc -> x ^"." ^ acc) (rev t ) (hole^h))
   ];
 *)
(**
fill_ident_with_prefix "meta_" <:ident< A.B.x>> |> p_ident_r;;
A.B.meta_x   
 *)    
value fill_ident_with_prefix ?(_loc=Loc.ghost) (hole:string) (ident:Ast.ident) =
  match List.rev (string_list_of_ident ident) with
    [ [] -> assert False
    | [h::t] -> List.(fold_right (fun x acc -> <:ident< $uid:x$ .  $acc$ >> )
			(rev t ) <:ident< $lid: hole^h$ >>)
   ];


(** f will be incremented for each item  indexed from 0 *)
value fold_left_i (f : 'b -> int -> 'a -> 'b ) (init:'b) (ty: list 'a)   : 'b =
    let (_,res) = List.fold_left  (fun (i,acc) ty ->
      (i+1, f ty i acc))   (0, init) ty
    in res;

(**
gen_patt_n 3 "x" <:patt< A _loc >> |> p_patt_r ;;
A _loc x0 x1 x2   
*)
value gen_patt_n
      ?(_loc=Loc.ghost) (n:int) (x:string) hole  : Ast.patt =
  fold_nat_left (fun acc i ->
		<:patt< $acc$ $lid:x^ string_of_int i$ >> ) hole n  
;

value patt_of_data_ctor_n
     ?(_loc=Loc.ghost) (n:int) (cons:Ast.ident)  :Ast.patt =
   gen_patt_n  ~_loc n "x" <:patt<$id:cons $ >>  ;
    

value gen_expr_n
      ?(_loc=Loc.ghost) (n:int) (x:string) hole  : Ast.expr =
  fold_nat_left (fun acc i ->
		<:expr< $acc$ $lid:x^ string_of_int i$ >> ) hole n  
;

value expr_of_data_ctor_n
      ?(_loc=Loc.ghost)  (n:int) (cons:Ast.ident)  : Ast.expr =
  gen_expr_n ~_loc n "x" <:expr< $id:cons$ >>  ;
    


value is_antiquot_data_ctor s = ends_with s "Ant" ;



(** Lift Ident
    value c = meta_ident <:ident< A.B >> = <:expr< <:ident< A.B >> >> ;
    value c : bool = True
*)  
value rec meta_ident  : Ast.ident -> Ast.expr  = fun
  [ <:ident@_loc< $i1$.$i2$ >> ->
    let u = meta_ident i1  and v = meta_ident i2 in
    <:expr< Ast.IdAcc _loc $u$ $v$ >>
  (** Written this way is due to the fact that camlp4 does not
     support ,, syntax  in common lisp *)
  | <:ident@_loc< $i1$ $i2$ >> ->
    let u = meta_ident i1 and v = meta_ident i2 in
    <:expr< Ast.IdApp  _loc $u$ $v$ >> 
  | <:ident@_loc< $anti:s$ >> ->
    <:expr< $anti:s$ >>
  | <:ident@_loc< $lid:s$ >> ->
    <:expr< Ast.IdLid _loc $str:s$ >>
  | <:ident@_loc< $uid:s$ >> ->
    <:expr< Ast.IdUid _loc $str:s$ >> 
  ]  
;


value me_of_id ?(_loc=Loc.ghost) (i:Ast.ident)  : Ast.expr = 
  <:expr< Ast.ExId _loc $meta_ident i$ >>
;    

value mp_of_id ?(_loc=Loc.ghost) (i:Ast.ident)  : Ast.expr = 
  <:expr< Ast.PaId _loc $meta_ident i$ >>
;

value failure    =
  let _loc = Loc.ghost in 
  <:expr< raise (Failure "metafilter: Cannot handle that kind of types ") >>
;       
value failure_loc _loc =
  <:expr< raise (Failure "metafilter: Cannot handle that kind of types ") >>
;  

(**
   value c =  <:expr< <:expr< $f$ $g$ >> >>  = me_app <:expr< f >> <:expr< g >> ;
   value c : bool = True
   value c= <:expr< <:expr< $f x $ $g$ >> >> = me_app <:expr< f x >> <:expr< g >> ;
   value c : bool = True 
*)
value me_app (x:Ast.expr) (y :Ast.expr) _loc : Ast.expr =
   <:expr< Ast.ExApp _loc $x$ $y$ >>
;     

value mp_app (x:Ast.expr) (y:Ast.expr) _loc : Ast.expr =
  <:expr< Ast.PaApp _loc $x$ $y$ >>      
;       




(** f takes data constructor name, a list of each arguments and then
    do monoid operation. Here we don't use wild pattern is for
    1. pendatic purpuse, 2. safety

    In revised syntax
    fold_data_ctors <:ctyp< [A of int | B of int]  >> (fun name _ _ -> print_string name )
    ();;
    AB
 *)
value fold_data_ctors (ty:Ast.ctyp)
      (f:string-> list Ast.ctyp -> 'a -> 'a )
      (init:'a) : 'a = (
	(** antiquotation list can not be recognized in pattern
	    language the same applies to `int, int they behave the
	    same *)
	let rec loop acc t = match t with
	    [ <:ctyp< $uid:cons$ of $tys$ >> ->
	      f cons (Ast.list_of_ctyp tys []) acc (* cons is data ctor *)
	    | <:ctyp< $uid:cons$ >> ->
	      f cons [] acc
	    | <:ctyp< $t1$ | $t2$ >> -> loop (loop acc t1) t2
	    | <:ctyp< >> -> acc
            (** ignore case below; written for documentation and safety *)		  
	    | <:ctyp< $id:_$ >> 
	    | <:ctyp< $_$ of $_$ >> 	      
	    | <:ctyp< $_$ as $_$ >> 
	    | <:ctyp< $anti:_$ >>
	    | <:ctyp< (module $_$ ) >> (** TyPkg *)
	    | <:ctyp< $_$ & $_$ >> (* conjunctive type in polymorphic variants *)
	    | <:ctyp< $_$ of & $_$ >> 
	    | <:ctyp< [ < $_$ ] >>
	    | <:ctyp< [ < $_$  > $_$ ] >> 
	    | <:ctyp< [ > $_$ ] >>
	    | <:ctyp< [ = $_$ ] >>
	    | <:ctyp< $_$ ; $_$ >>
	    | <:ctyp< $_$ , $_$ >>
	    | <:ctyp< $_$ : $_$ >>
	    | <:ctyp< $_$ * $_$ >>
	    | <:ctyp< $_$ and $_$ >>
            | <:ctyp< mutable $_$ >>
	    | <:ctyp< private $_$ >>
	    | <:ctyp<  $tup:_$  >>
	    | <:ctyp< $_$ == $_$ >>
	    | <:ctyp< [ $_$ ] >> (** TySum *)
	    | <:ctyp< { $_$ } >>
	    | <:ctyp< `$_$ >>
	    | <:ctyp< '$_$ >>
	    | <:ctyp< +'$_$ >>
	    | <:ctyp< -'$_$ >>
	    | <:ctyp< ! $_$ .  $_$ >>
	    | <:ctyp< ? $_$ : $_$  >> (** TyOlb *)
	    | <:ctyp< ~ $_$ : $_$ >> (** TyLab *)
	    | <:ctyp< <  $_$ $..:_$ > >> (** TyObj *)
	    | <:ctyp< # $_$ >>
	    | <:ctyp< $_$ -> $_$ >>
	    | <:ctyp< $_$ $_$ >>
	    | <:ctyp< _ >>
	    | Ast.TyDcl _ _ _ _ _    ->
			    do{
			      p_ctyp_r t;
			      assert False;
			    } ] in
  loop init ty
);


value rec handle_ctyp
    (ty:Ast.ctyp)
    (base:Ast.ident -> Ast.expr)
    (frag1,frag2 ): Ast.expr =
  match ty with
  [ <:ctyp@_loc< $id:id$ >> ->
    base id
  | <:ctyp@_loc< ( $tup:ts$ ) >> ->
    (* arbitrary tuple type *)  
      let ls = Ast.list_of_ctyp ts [] in
      let n = List.length ls in
      let arr = Array.init n (fun x -> x) in
      tuples n ls
  | <:ctyp@_loc< $t1$ $t2$ >> ->
      
      <:expr< $f_of_ctyp t1 (frag1,frag2) $ $f_of_ctyp t2 (frag1,frag2) $ >>
  | <:ctyp@_loc< '$s$ >> ->
      (* arbitrary type variable *)
      <:expr< $lid:mf_ s$ >>
  | _ -> failure_loc (Ast.loc_of_ctyp ty )
  ]
;

      
(**
   
 *)
value rec f_of_ctyp (ty:Ast.ctyp) (frag1,frag2 ): Ast.expr =
  match ty with
  [ <:ctyp@_loc< $id:id$ >> ->
    <:expr< $id: fill_ident_with_prefix ~_loc "meta_" id $ >>
    (* <:expr< $lid:(string_of_ident "meta_" id) $ >> *)
    (* ty -> meta_ty -- basic branch
       A.t -> A.meta_t 
     *)
  | <:ctyp@_loc< ( $tup:ts$ ) >> ->
    (* arbitrary tuple type *)  
      let ls = Ast.list_of_ctyp ts [] in
      let n = List.length ls in
      let arr = Array.init n (fun x -> x) in
      <:expr<
      fun _loc
	  $tup:
	  Array.( arr
                  |> mapi
		      (fun i _ ->
			<:patt< $id: x ~_loc i$ >> )
			(* <:patt< $lid:"x" ^ string_of_int i$ >> ) *)
		  |> to_list
		  |> Ast.paCom_of_list
           )$ ->
	     $ frag1  $
             $let res = 
	       (Array.(
		  ls
                  |> of_list
                  |> mapi (fun i t -> <:expr< $f_of_ctyp t (frag1,frag2) $ _loc $lid:"x" ^
		      string_of_int i $ >>)
		  |> to_list
	       )) in
               let app code1 code2 = <:expr< $code1$ $code2$ >>
	       in
               List.fold_left (fun acc code ->
			       app acc code )  frag2 res
             $
	       >>
  | <:ctyp@_loc< $t1$ $t2$ >> ->
      (* arbitray app type *)
      <:expr< $f_of_ctyp t1 (frag1,frag2) $ $f_of_ctyp t2 (frag1,frag2) $ >>
  | <:ctyp@_loc< '$s$ >> ->
      (* arbitrary type variable *)
      <:expr< $lid:mf_ s$ >>
  | _ -> failure_loc (Ast.loc_of_ctyp ty )
  ]
;
