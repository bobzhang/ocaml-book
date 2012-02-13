(** CAMLP4AST RevisedSyntax *)
type loc = Loc.t
and meta_bool =
  [ BTrue
  | BFalse
  | BAnt of string ]
and rec_flag =
  [ ReRecursive
  | ReNil
  | ReAnt of string ]
and direction_flag =
  [ DiTo
  | DiDownto
  | DiAnt of string ]
and mutable_flag =
  [ MuMutable
  | MuNil
  | MuAnt of string ]
and private_flag =
  [ PrPrivate
  | PrNil
  | PrAnt of string ]
and virtual_flag =
  [ ViVirtual
  | ViNil
  | ViAnt of string ]
and override_flag =
  [ OvOverride
  | OvNil
  | OvAnt of string ]
and row_var_flag =
  [ RvRowVar
  | RvNil
  | RvAnt of string ]
and meta_option 'a =
  [ ONone
  | OSome of 'a
  | OAnt of string ]
and meta_list 'a =
  [ LNil
  | LCons of 'a and meta_list 'a
  | LAnt of string ]


  



and ident =
  [ IdAcc of loc and ident and ident (* i . i *)
(*  <:ident< a . b >>   Access in module
   IdAcc of Loc.t and ident and ident *)
  | IdApp of loc and ident and ident (* i i *)
(*  <:ident< a b >>
   Application
   IdApp of Loc.t and ident and ident *)
  | IdLid of loc and string (* foo *)
(* <:ident< $lid:i$ >>
   Lowercase identifier
   IdLid of Loc.t and string
*)      
  | IdUid of loc and string (* Bar *)
(* <:ident< $uid:i$ >>
   Uppercase identifier
   IdUid of Loc.t and string
*)      
  | IdAnt of loc and string (* $s$ *)
(* <:ident< $anti:s$ >>
   Antiquotation
   IdAnt of Loc.t and string
*)      
 ]
(*  <:ident< $list:x$ >>
   list of accesses
   Ast.idAcc_of_list x  use IdAcc to accumulate to a list
 *)   
and ctyp =
  [ TyNil of loc
  (*<:ctyp< >>  Empty typeTyNil of Loc.t *)
  | TyAli of loc and ctyp and ctyp (* t as t *) (* list 'a as 'a *)
  (* <:ctyp< t as t >>  Type aliasing
     TyAli of Loc.t and ctyp and ctyp *)
  | TyAny of loc (* _ *)
  (*  <:ctyp< _ >> Wildcard TyAny of Loc.t *)
  | TyApp of loc and ctyp and ctyp (* t t *) (* list 'a *)
  (* <:ctyp< t t >> Application TyApp of Loc.t and ctyp and ctyp *)
  | TyArr of loc and ctyp and ctyp (* t -> t *) (* int -> string *)
  (* <:ctyp< t -> t >>Arrow TyArr of Loc.t and ctyp and ctyp       *)
  | TyCls of loc and ident (* #i *) (* #point *)
  (* <:ctyp< #i >> Class type TyCls of Loc.t and ident     *)
  | TyLab of loc and string and ctyp (* ~s:t *)
  (* <:ctyp< ~s >>  Label type TyLab of Loc.t and string and ctyp    *)
  | TyId  of loc and ident (* i *) (* Lazy.t *)
  (* <:ctyp< $id:i$ >>  Type identifier of TyId of Loc.t and ident *)
  (* <:ctyp< $lid:i$ >> TyId (_, IdLid (_, i)) *)
  (* <:ctyp< $uid:i$ >>  TyId (_, IdUid (_, i)) *)
  | TyMan of loc and ctyp and ctyp (* t == t *) (* type t = [ A | B ] == Foo.t *)
  (* <:ctyp< t == t >>  Type manifest TyMan of Loc.t and ctyp and ctyp       *)

  (* type t 'a 'b 'c = t constraint t = t constraint t = t *)
  | TyDcl of loc and string and list ctyp and ctyp and list (ctyp * ctyp)
  (* <:ctyp< type t 'a 'b 'c = t constraint t = t constraint t = t >>
     Type declaration
     TyDcl of Loc.t and string and list ctyp and ctyp and list (ctyp * ctyp) *)
      
  (* < (t)? (..)? > *) (* < move : int -> 'a .. > as 'a  *)
  | TyObj of loc and ctyp and row_var_flag
  (* <:ctyp< < (t)? (..)? > >> Object type TyObj of Loc.t and ctyp and meta_bool       *)
      
  | TyOlb of loc and string and ctyp (* ?s:t *)
  (* <:ctyp< ?s:t >>  Optional label type TyOlb of Loc.t and string and ctyp       *)
  | TyPol of loc and ctyp and ctyp (* ! t . t *) (* ! 'a . list 'a -> 'a *)
  (* <:ctyp< ! t . t >> = Polymorphic type TyPol of Loc.t and ctyp and ctyp       *)
  | TyQuo of loc and string (* 's *)
  (* <:ctyp< 's >>' TyQuo of Loc.t and string       *)
  | TyQuP of loc and string (* +'s *)
  (* <:ctyp< +'s >> TyQuP of Loc.t and string       *)
  | TyQuM of loc and string (* -'s *)
  (* <:ctyp< -'s >> TyQuM of Loc.t and string       *)
  | TyVrn of loc and string (* `s *)
  (* <:ctyp< `s >> Polymorphic variant of TyVrn of Loc.t and string       *)
  | TyRec of loc and ctyp (* { t } *) (* { foo : int ; bar : mutable string } *)
  (* <:ctyp< { t } >>  Record TyRec of Loc.t and ctyp *)

  | TyCol of loc and ctyp and ctyp (* t : t *)
  (* <:ctyp< t : t >>Field declarationTyCol of Loc.t and ctyp and ctyp       *)
  | TySem of loc and ctyp and ctyp (* t; t *)
  (* <:ctyp< t; t >>Semicolon-separated type listTySem of Loc.t and ctyp and ctyp       *)
  | TyCom of loc and ctyp and ctyp (* t, t *)
  (* <:ctyp< t, t >>Comma-separated type listTyCom of Loc.t and ctyp and ctyp *)
  | TySum of loc and ctyp (* [ t ] *) (* [ A of int and string | B ] *)
  (* <:ctyp< [ t ] >>Sum typeTySum of Loc.t and ctyp       *)
  | TyOf  of loc and ctyp and ctyp (* t of t *) (* A of int *)
  (* <:ctyp< t of t >>TyOf of Loc.t and ctyp and ctyp       *)
  | TyAnd of loc and ctyp and ctyp (* t and t *)
  (* <:ctyp< t and t >>TyAnd of Loc.t and ctyp and ctyp *)
  | TyOr  of loc and ctyp and ctyp (* t | t *)
  (* <:ctyp< t | t >>"Or" pattern between typesTyOr of Loc.t and ctyp and ctyp       *)
  | TyPrv of loc and ctyp (* private t *)
  (* <:ctyp< private t >>Private type TyPrv of Loc.t and ctyp       *)
  | TyMut of loc and ctyp (* mutable t *)
  (* <:ctyp< mutable t >>  Mutable type TyMut of Loc.t and ctyp       *)
  | TyTup of loc and ctyp (* ( t ) *) (* (int * string) *)
  (* <:ctyp< ( t ) >> or <:ctyp< $tup:t$ >>  Tuple of TyTup of Loc.t and ctyp       *)
  | TySta of loc and ctyp and ctyp (* t * t *)
  (* <:ctyp< t * t >> TySta of Loc.t and ctyp and ctyp       *)
  | TyVrnEq of loc and ctyp (* [ = t ] *)
  (* <:ctyp< [ = t ] >>  TyVrnEq of Loc.t and ctyp       *)
  | TyVrnSup of loc and ctyp (* [ > t ] *)
  (* <:ctyp< [ > t ] >> open polymorphic variant type TyVrnSup of Loc.t and ctyp       *)
  | TyVrnInf of loc and ctyp (* [ < t ] *)
  (* <:ctyp< [ < t ] >>  closed polymorphic variant type with no known tags
      TyVrnInf of Loc.t and ctyp       *)
  | TyVrnInfSup of loc and ctyp and ctyp (* [ < t > t ] *)
  (*  <:ctyp< [ < t > t ] >> closed polymorphic variant type with some known tags
      TyVrnInfSup of Loc.t and ctyp and ctyp
  *)
  | TyAmp of loc and ctyp and ctyp (* t & t *)
  (* <:ctyp< t & t >>conjuntive type in polymorphic variants
     TyAmp of Loc.t and ctyp and ctyp   *)
  | TyOfAmp of loc and ctyp and ctyp (* t of & t *)
  (* <:ctyp< $t1$ of & $t2$ >>Special (impossible) constructor (t1)
     that has both no arguments and arguments compatible with t2 at the
     same time.TyOfAmp of Loc.t and ctyp and ctyp *)
  | TyPkg of loc and module_type (* (module S) *)
  (* <:ctyp<(module S) >>  TyPkg of loc and module_type    *)
  | TyAnt of loc and string (* $s$ *)
  (* <:ctyp< $anti:s$ >>AntiquotationTyAnt of Loc.t and string       *)
  (*<:ctyp< $list:x$ >>list of accumulated ctyps
  depending on context,
  Ast.tyAnd_of_list,
  Ast.tySem_of_list,
  Ast.tySta_of_list,
  Ast.tyOr_of_list,
  Ast.tyCom_of_list,
  Ast.tyAmp_of_list

  In a closed variant type <:ctyp< [ < $t1$ > $t2$ ] >> the type t2 must
    not be the empty type; use a TyVrnInf node in this case.

  Type conjuctions are stored in a TyAmp tree, use Camlp4Ast.list_of_ctyp and
  Camlp4Ast.tyAmp_of_list to convert from and to a list of types.

  Variant constructors with arguments and polymorphic variant
  constructors with arguments are both represented with a TyOf
  node. For variant types the first TyOf type is an uppercase
  identifier (TyId), for polymorphic variant types it is an TyVrn
  node.

  Constant variant constructors are simply represented as
  uppercase identifiers (TyId). Constant polymorphic variant
  constructors take a TyVrn node.  *)
]
and patt =
  [ PaNil of loc
  (* <:patt< >>Empty patternPaNil of Loc.t *)
  | PaId  of loc and ident (* i *)
  (* <:patt< $id:i$ >>IdentifierPaId of Loc.t and ident       *)
  (* <:patt< $lid:i$ >>PaId (_, IdLid (_, i)) *)
  (* <:patt< $uid:i$ >>PaId (_, IdUid (_, i)) *)
  | PaAli of loc and patt and patt (* p as p *) (* (Node x y as n) *)
  (* <:patt< ( p as p ) >>AliasPaAli of Loc.t and patt and patt       *)
  | PaAnt of loc and string (* $s$ *)
  (* <:patt< $anti:s$ >>AntiquotationPaAnt of Loc.t and string       *)
  | PaAny of loc (* _ *)
  (* <:patt< _ >>WildcardPaAny of Loc.t       *)
  | PaApp of loc and patt and patt (* p p *) (* fun x y -> *)
  (* <:patt< p p >>ApplicationPaApp of Loc.t and patt and patt       *)
  | PaArr of loc and patt (* [| p |] *)
  (* <:patt< [| p |] >>ArrayPaArr of Loc.t and patt       *)
  | PaCom of loc and patt and patt (* p, p *)
  (* <:patt< p, p >>Comma-separated pattern listPaCom of Loc.t and patt and patt       *)
  | PaSem of loc and patt and patt (* p; p *)
  (* <:patt< p; p >>Semicolon-separated pattern listPaSem of Loc.t and patt and patt       *)
  | PaChr of loc and string (* c *) (* 'x' *)
  (* <:patt< $chr:c$ >>CharacterPaChr of Loc.t and string       *)
  | PaInt of loc and string
  (* <:patt< $int:i$ >>IntegerPaInt of Loc.t and string       *)
  | PaInt32 of loc and string
  (* <:patt< $int32:i$ >>Int32PaInt32 of Loc.t and string       *)
  | PaInt64 of loc and string
  (* <:patt< $int64:i$ >>Int64PaInt64 of Loc.t and string       *)
  | PaNativeInt of loc and string
  (* <:patt< $nativeint:i$ >>NativeIntPaNativeInt of Loc.t and string       *)
  | PaFlo of loc and string
  (* <:patt< $flo:f$ >>FloatPaFlo of Loc.t and string       *)
  | PaLab of loc and string and patt (* ~s or ~s:(p) *)
  (* <:patt< ~s >> <:patt< s:(p) >>LabelPaLab of Loc.t and string and patt       *)

  (* ?s or ?s:(p) *)
  | PaOlb of loc and string and patt
  (* <:patt< ?s >> <:patt< ?s:(p) >>Optional labelPaOlb of Loc.t and string and patt       *)

  (* ?s:(p = e) or ?(p = e) *)
  | PaOlbi of loc and string and patt and expr
  (* <:patt< ?s:(p = e) >> <:patt< ?(p = e) >
     >Optional label with default valuePaOlbi of Loc.t and string and patt and expr       *)

  | PaOrp of loc and patt and patt (* p | p *)
  (* <:patt< p | p >>OrPaOrp of Loc.t and patt and patt       *)
  | PaRng of loc and patt and patt (* p .. p *)
  (* <:patt< p .. p >>Pattern rangePaRng of Loc.t and patt and patt *)
  | PaRec of loc and patt (* { p } *)
  (* <:patt< { p } >>RecordPaRec of Loc.t and patt *)
  | PaEq  of loc and ident and patt (* i = p *)
  (* <:patt< i = p >>EqualityPaEq of Loc.t and ident and patt       *)
  | PaStr of loc and string (* s *)
  (* <:patt< $str:s$ >>StringPaStr of Loc.t and string *)
  | PaTup of loc and patt (* ( p ) *)
  (* <:patt< ( $tup:p$ ) >>TuplePaTup of Loc.t and patt       *)
  | PaTyc of loc and patt and ctyp (* (p : t) *)
  (* <:patt< (p : t) >>Type constraintPaTyc of Loc.t and patt and ctyp       *)
  | PaTyp of loc and ident (* #i *)
  (* <:patt< #i >>PaTyp of Loc.t and ident
     used in polymorphic variants
  *)
  | PaVrn of loc and string (* `s *)
  (* <:patt< `s >>Polymorphic variantPaVrn of Loc.t and string   *)
  | PaLaz of loc and patt (* lazy p *)
  (* <:patt< lazy x >> *)      
 ]
 (* <:patt< $list:x$ >>list of accumulated patts depending on context,
    Ast.paCom_of_list, Ast.paSem_of_list Tuple elements are wrapped in a
    PaCom tree. The utility functions Camlp4Ast.paCom_of_list and
    Camlp4Ast.list_of_patt convert from and to a list of tuple
    elements.  *)
and expr =
  [ ExNil of loc
      (* <:expr< >> *)
  | ExId  of loc and ident (* i *)
      (* <:expr< $id:i$ >> notice that antiquot id requires ident directly *)
      (* <:expr< $lid:i$ >> ExId(_,IdLid(_,i)) *)
      (* <:expr< $uid:i$ >> ExId(_, IdUid(_,i)) *)
  | ExAcc of loc and expr and expr (* e.e *)
      (* <:expr< $e1$.$e2$ >> Access in module ? *)
  | ExAnt of loc and string (* $s$ *)
      (* <:expr< $anti:s$ >> *)
  | ExApp of loc and expr and expr (* e e *)
      (* <:expr< $e1$ $e2$ >> Application *)
  | ExAre of loc and expr and expr (* e.(e) *)
      (* <:expr< $e$.($e$) >> Array access  *)
  | ExArr of loc and expr (* [| e |] *)
      (* <:expr< [|$e$| ] Array declaration *)
  | ExSem of loc and expr and expr (* e; e *)
      (* <:expr< $e$; $e$ >>  *)
  | ExAsf of loc (* assert False *)
      (* <:expr< assert False >> *)
  | ExAsr of loc and expr (* assert e *)
      (* <:expr< assert $e$ >>  *)
  | ExAss of loc and expr and expr (* e := e *)
      (* <:expr< $e$ := $e$ >>  *)
  | ExChr of loc and string (* 'c' *)
      (* <:exp< $`chr:s$ >> Character *)
  | ExCoe of loc and expr and ctyp and ctyp (* (e : t) or (e : t :> t) *)
      (* <:expr< ($e$:> $t$) >> <:expr< ($e$ : $t1$ :> $t2$ ) >>
	 The first ctyp is populated by TyNil
      *)
  | ExFlo of loc and string (* 3.14 *)
      (* <:expr< $flo:f$ >> *)
      (* <:expr< $`flo:f$ >>  ExFlo(_,string_of_float f) *)

  (* for s = e to/downto e do { e } *)
  | ExFor of loc and string and expr and expr and direction_flag and expr
      (* <:expr< for $s$ = $e1$ to/downto $e2$ do { $e$  } >>  *)
  | ExFun of loc and match_case (* fun [ mc ] *)
      (* <:expr< fun [ $a$ ] >>  *)
  | ExIfe of loc and expr and expr and expr (* if e then e else e *)
      (* <:expr< if $e$ then $e$ else $e$ >>  *)
  | ExInt of loc and string (* 42 *)
      (* <:expr< $int:i$ >> *)
      (* <:expr< $`int:i$ >> ExInt(_, string_of_int i) *)
  | ExInt32 of loc and string
      (* <:expr< $int32:i$ >>
	 <:expr< $`int32:i$ >>
      *)
  | ExInt64 of loc and string
      (* <:expr< $int64:i$ >> *)
      (* <:expr< $`int64:i$ >>  *)
  | ExNativeInt of loc and string
      (* <:expr< $nativeint:i$ >> <:expr< $`nativeint:i$ >>  *)
  | ExLab of loc and string and expr (* ~s or ~s:e *)
      (* <:expr< ~ $s$ >> ExLab (_, s, ExNil) *)
      (* <:expr< ~ $s$ : $e$ >>  *)
  | ExLaz of loc and expr (* lazy e *)
      (* <:expr< lazy $e$ >>  *)

  
  | ExLet of loc and rec_flag and binding and expr
      (* <:expr< let $b$ in $e$ >> *)
      (* <:expr< let rec $b$ in $e$ >>    *)

  | ExLmd of loc and string and module_expr and expr
      (* <:expr< let module $s$ = $me$ in $e$ >>  *)


  | ExMat of loc and expr and match_case
      (* <:expr< match $e$ with [ $a$ ] >>  *)

  (* new i *)
  | ExNew of loc and ident
      (* <:expr< new $id:i$ >>  new object *)
      (* <:expr< new $lid:str$ >>  *)

  (* object ((p))? (cst)? end *)
  | ExObj of loc and patt and class_str_item
      (* <:expr< object ( ($p$))? ($cst$)? end >>  object declaration *)

  (* ?s or ?s:e *)
  | ExOlb of loc and string and expr
      (* <:expr< ? $s$ >>      Optional label  *)
      (* <:expr< ? $s$ : $e$ >>  *)
  
  | ExOvr of loc and rec_binding
  (* <:expr< {< $rb$  >} >>  *)

  | ExRec of loc and rec_binding and expr
      (* <:expr< { $b$ } >> *)
      (* <:expr< {($e$ ) with $b$ } >>  *)
  | ExSeq of loc and expr
      (* <:expr< do { $e$ } >> *)
      (* <:expr< $seq:e$ >> *)
      (* another way to help you figure out the type *)
      (* type let f e = <:expr< $seq:e$ >> in the toplevel 	   *)

  | ExSnd of loc and expr and string
      (* <:expr< $e$ # $s$ >> METHOD call  *)

  | ExSte of loc and expr and expr
      (* <:expr< $e$.[$e$] >> String access  *)

  | ExStr of loc and string
      (* <:expr< $str:s$ >> "\n" -> "\n" *)
      (* <:expr< $`str:s$ >> "\n" -> "\\n" *)

  | ExTry of loc and expr and match_case
      (* <:expr< try $e$ with [ $a$ ] >>  *)


  | ExTup of loc and expr
      (* <:expr< ( $tup:e$ ) >>  *)

  | ExCom of loc and expr and expr
      (* <:expr< $e$, $e$ >>  *)

  (* (e : t) *)
  | ExTyc of loc and expr and ctyp
      (* <:expr< ($e$ : $t$ ) Type constraint  *)

  | ExVrn of loc and string
      (* <:expr< `$s$ >>  *)

  | ExWhi of loc and expr and expr
      (* <:expr< while $e$ do { $e$ } >>  *)

  | ExOpI of loc and ident and expr
      (* <:expr< let open $id:i$ in $e$ >>  *)

  | ExFUN of loc and string and expr
      (* <:expr< fun (type $s$ ) -> $e$  >> *)
      (* let f x (type t) y z = e *)

  | ExPkg of loc and module_expr
     (* (module ME : S) which is represented as (module (ME : S)) *)
  ]
  and module_type =
    (**
       mt ::=
       | (* empty *)
       | ident
       | functor (s : mt) -> mt
       | 's
       | sig sg end
       | mt with wc
       | $s$
    *)
    [ MtNil of loc
      
    | MtId  of loc and ident
    (* i *) (* A.B.C *)
    (* <:module_type< $id:ident$ >> named module type  *)

    | MtFun of loc and string and module_type and module_type
    (* <:module_type< functor ($uid:s$ : $mtyp:mta$ ) -> $mtyp:mtr$ >> *)
          
    | MtQuo of loc and string
    (* 's *)

    | MtSig of loc and sig_item
    (* sig sg end *)
    (* <:module_type< sig $sigi:sig_items$ end >>  *)
	
    | MtWit of loc and module_type and with_constr
    (* mt with wc *)
    (* <:module_type< $mtyp:mt$ with $with_constr:with_contr$ >>  *)
    | MtOf of loc and module_expr
    (* module type of m *)

    | MtAnt of loc and string
    (* $s$ *)
]
  (** Several with-constraints are stored in an WcAnd tree. Use
      Ast.wcAnd_of_list and Ast.list_of_with_constr to convert from and to a
      list of with-constraints.  Several signature items are stored in an
      SgSem tree. Use Ast.sgSem_of_list and Ast.list_of_sig_item to convert
      from and to a list of signature items. *)
and sig_item =
    (*
       sig_item, sg ::=
   | (* empty *)
   | class cict
   | class type cict
   | sg ; sg
   | #s
   | #s e
   | exception t
   | external s : t = s ... s
   | include mt
   | module s : mt
   | module rec mb
   | module type s = mt
   | open i
   | type t
   | value s : t
   | $s$
   lacking documentation !!   
    *)
  [ SgNil of loc
      
    (* class cict *)
    (* <:sig_item< class $s$ >>;; *)
    (* <:sig_item< class $typ:s$ >>;; *)
    | SgCls of loc and class_type

    (* class type cict *)
    (* <:sig_item< class type $s$ >>;; *)
    (* <:sig_item< class type $typ:s$ >>;;  *)
    | SgClt of loc and class_type

    (* sg ; sg *)
    | SgSem of loc and sig_item and sig_item

    (* # s or # s e ???  *)
    (* Directive  *)
    | SgDir of loc and string and expr

    (* exception t *)
    | SgExc of loc and ctyp

    (* external s : t = s ... s *)
    (*  <:sig_item< external $lid:id$ : $typ:type$ = $str_list:string_list$ >>
	another antiquot str_list 
    *)  
    | SgExt of loc and string and ctyp and meta_list string

    (* include mt *)
    | SgInc of loc and module_type

    (* module s : mt *)
    (* <:sig_item< module $uid:id$ : $mtyp:mod_type$ >>
       module Functor declaration 
    *)
    (**
       <:sig_item< module $uid:mid$ ( $uid:arg$ : $mtyp:arg_type$ ) : $mtyp:res_type$ >>
       -->
       <:sig_item< module $uid:mid$ : functor ( $uid:arg$ : $mtyp:arg_type$ ) -> $mtyp:res_type$ >>
    *)  
    | SgMod of loc and string and module_type

    (* module rec mb *)
    | SgRecMod of loc and module_binding

    (* module type s = mt *)
    (* <:sig_item< module type $uid:id$ = $mtyp:mod_type$ >>
       module type declaration 
    *)
    (**
       <:sig_item< module type $uid:id$ >> abstract module type
       -->
       <:sig_item< module type $uid:id$ = $mtyp:<:module_type< >>$ >>
    *)  
    | SgMty of loc and string and module_type

    (* open i *)
    | SgOpn of loc and ident

    (* type t *)
    (* <:sig_item< type $typ:type$ >>  *)
   (** <:sig_item< type $lid:id$ $p1$ ... $pn$ = $t$ constraint $c1l$
       = $c1r$ ... constraint $cnl$ = $cnr$ >>

       type declaration
       SgTyp
       of Loc.t and (TyDcl of Loc.t and id and [p1;...;pn] and t and
       [(c1l, c1r); ... (cnl, cnr)]) *)
   | SgTyp of loc and ctyp

    (* value s : t *)
    (* <:sig_item< value $lid:id$ : $typ:type$ >>  *)
    | SgVal of loc and string and ctyp

    | SgAnt of loc and string (* $s$ *) ]
  (** An exception is treated like a single type constructor. For
      exception declarations the type should be either a type
      identifier (TyId) or a type constructor (TyOf).

      Abstract module type declarations (i.e., module type
      declarations without equation) are represented with the empty
      module type.

      Mutually recursive type declarations (separated by
      and) are stored in a TyAnd tree. Use Ast.list_of_ctyp and
      Ast.tyAnd_of_list to convert to and from a list of type
      declarations.

      The quotation parser for types (<:ctyp< ... >>) does not parse
      type declarations. Type declarations must therefore be embedded
      in a sig_item or str_item quotation.

      There seems to be no antiquotation syntax for a list of type
      parameters and a list of constraints inside a type
      declaration. The existing form can only be used for a fixed
      number of type parameters and constraints.

      Complete class and class type declarations (including name and
      type parameters) are stored as class types.

      Several "and" separated class or class type declarations are
      stored in a CtAnd tree, use Ast.list_of_class_type and
      Ast.ctAnd_of_list to convert to and from a list of class
      types.  *)
and with_constr =
    (**
       with_constraint, with_constr, wc ::=
   | wc and wc
   | type t = t
   | module i = i
    *)
  [ WcNil of loc

    (* type t = t *)
    (* <:with_constr< type $typ:type_1$ = $typ:type_2$ >>  *)
    | WcTyp of loc and ctyp and ctyp

    (* module i = i *)
    (* <:with_constr< module $id:ident_1$ = $id:ident_2$ >>  *)
    | WcMod of loc and ident and ident

    (* type t := t *)
    | WcTyS of loc and ctyp and ctyp

    (* module i := i *)
    | WcMoS of loc and ident and ident

    (* wc and wc *)
    (* <:with_constr< $with_constr:wc1$ and $with_constr:wc2$ >> *)
    | WcAnd of loc and with_constr and with_constr

    | WcAnt of loc and string (* $s$ *)
]
  (** Several with-constraints are stored in an WcAnd tree. Use
      Ast.wcAnd_of_list and Ast.list_of_with_constr to convert from and
      to a list of with-constraints. *)
and binding =
  (**  binding, bi ::=
   | bi and bi
   | p = e *)
  [ BiNil of loc

    (* bi and bi *) (* let a = 42 and c = 43 *)
    (* <:binding< $b1$ and $b2$ >>  *)
    | BiAnd of loc and binding and binding

    (* p = e *) (* let patt = expr *)
    (* <:binding< $pat:pattern$ = $exp:expression$ >>
       <:binding< $p$ = $e$ >> ;; both are ok
    *)
    (**
       <:binding< $pat:f$ $pat:x$ = $exp:ex$ >>
       -->
       <:binding< $pat:f$ = fun $pat:x$ -> $exp:ex$ >>

       <:binding< $pat:p$ : $typ:type$ = $exp:ex$ >>
       typed binding --> 
       <:binding< $pat:p$ = ( $exp:ex$ : $typ:type$ ) >>

       <:binding< $pat:p$ :> $typ:type$ = $exp:ex$ >>
       coercion binding -->
       <:binding< $pat:p$ = ( $exp:ex$ :> $typ:type$ ) >>
    *)  
    | BiEq  of loc and patt and expr

    | BiAnt of loc and string (* $s$ *)
    (**
       The utility functions Camlp4Ast.biAnd_of_list and
       Camlp4Ast.list_of_bindings convert between the BiAnd tree of
       parallel bindings and a list of bindings.  The utility
       functions Camlp4Ast.binding_of_pel and pel_of_binding convert
       between the BiAnd tree and a pattern * expression lis
    *)  ]
and rec_binding =
    (** record bindings
	record_binding, rec_binding, rb ::=
	| rb ; rb
	| x = e
    *)
  [ RbNil of loc

    (* rb ; rb *)
    | RbSem of loc and rec_binding and rec_binding

    (* i = e
       very simple 
    *)
    | RbEq  of loc and ident and expr

    | RbAnt of loc and string (* $s$ *)
    ]
    
  and module_binding =
    (**
       Recursive module bindings 
       module_binding, mb ::=
       | (* empty *)
       | mb and mb
       | s : mt = me
       | s : mt
       | $s$
    *)
    [ MbNil of loc

    (* mb and mb *) (* module rec (s : mt) = me and (s : mt) = me *)
    | MbAnd of loc and module_binding and module_binding

    (* s : mt = me *)
    | MbColEq  of loc and string and module_type and module_expr

    (* s : mt *)
    | MbCol  of loc and string and module_type

    | MbAnt of loc and string (* $s$ *) ]

and match_case =
    (**
       match_case, mc ::=
       | (* empty *)
       | mc | mc
       | p when e -> e
       | p -> e
    (* a sugar for << p when e1 -> e2 >> where e1 is the empty expression *)
    <:match_case< $list:mcs$ >>list of or-separated match casesAst.mcOr_of_list
    *)
    [
      (* <:match_case< >>  *)
      McNil of loc

    (* a | a *)
    (* <:match_case< $mc1$ | $mc2$ >> 	   *)
    | McOr of loc and match_case and match_case

    (* p (when e)? -> e *)
    (* <:match_case< $p$ -> $e$ >>    *)
    (* <:match_case< $p$ when $e1$ or $e2$ >>    *)
    | McArr of loc and patt and expr and expr

    (* <:match_case< $anti:s$ >>    *)
    | McAnt of loc and string (* $s$ *) ]

and module_expr =
    (**
        module_expression, module_expr, me ::=
       | (* empty *)
       | ident
       | me me
       | functor (s : mt) -> me
       | struct st end
       | (me : mt)
       | $s$
       | (value pexpr : ptype)
    *)
    [
      (* <:module_expr< >>  *)
      MeNil of loc

    (* i *)
    (* <:module_expr< $id:mod_ident$ >>  *)
    | MeId  of loc and ident

    (* me me *)
    (* <:module_expr< $mexp:me$ $mexp:me$ >> 	Functor application *)
    | MeApp of loc and module_expr and module_expr

    (* functor (s : mt) -> me *)
    (* <:module_expr< functor ($uid:id$ : $mtyp:mod_type$) -> $mexp:me$ >>  *)
    | MeFun of loc and string and module_type and module_expr

    (* struct st end *)
   (* <:module_expr< struct $stri:str_item$ end >>  *)
    | MeStr of loc and str_item

    (* (me : mt) *)
    (* <:module_expr< ($mexp:me$ : $mtyp:mod_type$ ) >>
       signature constraint
    *)
    | MeTyc of loc and module_expr and module_type

    (* (value e) *)
    (* (value e : S) which is represented as (value (e : S)) *)
    (* <:module_expr< (value $exp:expression$ ) >>
       module extraction

       <:module_expr< (value $exp:expression$ : $mtyp:mod_type$ ) >>
       -->
       <:module_expr<
       ( value $exp: <:expr< ($exp:expression$ : (module $mtyp:mod_type$ ) ) >> $ )
       >>
    *)
    | MePkg of loc and expr

    (* <:module_expr< $anti:string$ >> 	 *)
    | MeAnt of loc and string (* $s$ *)

(** Inside a structure several structure items are packed into a StSem
    tree. Use Camlp4Ast.stSem_of_list and Camlp4Ast.list_of_str_item to
    convert from and to a list of structure items.
    
    The expression in a module extraction (MePkg) must be a type
    constraint with a package type. Internally the syntactic class of
    module types is used for package types.
*)
]
  and str_item =
    (**
        structure_item, str_item, st ::=
   | (* empty *)
   | class cice
   | class type cict
   | st ; st
   | #s
   | #s e
   | exception t or exception t = i
   | e
   | external s : t = s ... s
   | include me
   | module s = me
   | module rec mb
   | module type s = mt
   | open i
   | type t
   | value b or value rec bi
   | $s$
    *)
    [ StNil of loc

    (* class cice *)
    (* <:str_item< class $cdcl:class_expr$ >>  *)
    | StCls of loc and class_expr

    (* class type cict *)
    (**
      <:str_item< class type $typ:class_type$ >>
       --> class type definition 
    *) 
    | StClt of loc and class_type

    (* st ; st *)
    (* <:str_item< $str_item_1$; $str_item_2$ >>  *)
    | StSem of loc and str_item and str_item

    (* # s or # s e *)
    (* <:str_item< # $string$ $expr$ >>  *)
    | StDir of loc and string and expr

    (* exception t or exception t = i *)
    (* <:str_item< exception $typ:type$ >>   -> None *)
    (* <:str_item< exception $typ:type$ >> ->
       Exception alias -> Some ident
    *)	
    | StExc of loc and ctyp and meta_option(*FIXME*) ident

    (* e *)
    (* <:str_item< $exp:expr$ >> toplevel expression 	 *)
    | StExp of loc and expr

    (* external s : t = s ... s *)
    (* <:str_item< external $lid:id$ : $typ:type$ = $str_list:string_list$ >>  *)
    | StExt of loc and string and ctyp and meta_list string

    (* include me *)
    (* <:str_item< include $mexp:mod_expr$ >>  *)
    | StInc of loc and module_expr

    (* module s = me *)
    (* <:str_item< module $uid:id$ = $mexp:mod_expr$ >>  *)
    | StMod of loc and string and module_expr

    (* module rec mb *)
    (* <:str_item< module rec $module_binding:module_binding$ >>  *)
    | StRecMod of loc and module_binding

    (* module type s = mt *)
    (* <:str_item< module type $uid:id$ = $mtyp:mod_type$ >> *)
    | StMty of loc and string and module_type

    (* open i *)
    (* <:str_item< open $id:ident$ >>  *)
    | StOpn of loc and ident

    (* type t *)
    (* <:str_item< type $typ:type$ >>  *)
    (**
       <:str_item< type $lid:id$ $p1$ ... $pn$ = $t$
       constraint $c1l$ = $c1r$ ... constraint $cnl$ = $cnr$ >>
       -->
       StTyp of Loc.t and
       (TyDcl of Loc.t and id and [p1;...;pn] and t and [(c1l, c1r); ... (cnl, cnr)])
    *)
    | StTyp of loc and ctyp

    (* value (rec)? bi *)
    (* <:str_item< value $rec:r$ $binding$ >> 	 *)
    (* <:str_item< value rec $binding$ >> *)
    (* <:str_item< value  $binding$ >> *)
    | StVal of loc and rec_flag and binding

    (* <:str_item< $anti:s$ >> 	 *)
    | StAnt of loc and string (* $s$ *)
(**
   <:str_item< module $uid:id$ ( $uid:p$ : $mtyp:mod_type$ ) = $mexp:mod_expr$ >>
   ---->
   <:str_item< module $uid:id$ =
   functor ( $uid:p$ : $mtyp:mod_type$ ) -> $mexp:mod_expr$ >>

   <:str_item< module $uid:id$ : $mtyp:mod_type = $mexp:mod_expr$ >>
   ---->
   <:str_item< module $uid:id$ = ($mexp:mod_expr$ : $mtyp:mod_type ) >>

   <:str_item< type t >>
   ---->
   <:str_item< type t = $<:ctyp< >>$ >>

   <:str_item< # $id$ >> (directive without arguments)
   ---->
   <:str_item< # $a$ $<:expr< >>$ >>


   A whole compilation unit or the contents of a structure is given as
   *one* structure item in the form of a StSem tree.

   The utility functions Camlp4Ast.stSem_of_list and
   Camlp4Ast.list_of_str_item convert from and to a list of structure
   items.

   An exception is treated like a single type constructor. For
   exception definitions the type should be either a type identifier
   (TyId) or a type constructor (TyOf). For execption aliases it
   should only be a type identifier (TyId).

   Abstract types are represented with the empty type.

   Mutually recursive type definitions (separated by and) are stored
   in a TyAnd tree. Use Ast.list_of_ctyp and Ast.tyAnd_of_list to
   convert to and from a list of type definitions.

   The quotation parser for types (<:ctyp< ... >>) does not parse type
   declarations. Type definitions must therefore be embedded in a
   sig_item or str_item quotation.

   There seems to be no antiquotation syntax for a list of type
   parameters and a list of constraints inside a type definition. The
   existing form can only be used for a fixed number of type
   parameters and constraints.

   Complete class type definitions (including name and type
   parameters) are stored as class types.

   Several "and" separated class type definitions are stored in a
   CtAnd tree, use Ast.list_of_class_type and Ast.ctAnd_of_list to
   convert to and from a list of class types.

   Several "and" separated classes are stored in a CeAnd tree, use
   Ast.list_of_class_exprand Ast.ceAnd_of_list to convert to and from
   a list of class expressions.

   Several "and" separated recursive modules are stored in a MbAnd
   tree, use Ast.list_of_module_binding and Ast.mbAnd_of_list to
   convert to and from a list of module bindings.

   Directives without argument are represented with the empty
   expression argument.  *)
]
and class_type =
  (** Besides class types, ast nodes of this type are used to
      describe *class type definitions*
      (in structures and signatures)
      and class declarations (in signatures).

      class_type, ct ::=
   | (* empty *)
   | (virtual)? i ([ t ])?
   | [t] -> ct
   | object (t) csg end
   | ct and ct
   | ct : ct
   | ct = ct
   | $s$
    *)
  [ CtNil of loc

    (* (virtual)? i ([ t ])? *)
    (* <:class_type< $virtual:v$ $id:ident$ [$list:p$ ] >> *)
    (* instanciated class type/ left hand side of a class *)
    (* declaration or class type definition/declaration  *)
    | CtCon of loc and virtual_flag and ident and ctyp

    (* [t] -> ct *)
    (* <:class_type< [$typ:type$] -> $ctyp:ct$ >>
	 class type valued function 
    *)
    | CtFun of loc and ctyp and class_type

    (* object ((t))? (csg)? end *)
    (* <:class_type< object ($typ:self_type$) $csg:class_sig_item$ end >> *)
    (* class body type *)
    | CtSig of loc and ctyp and class_sig_item

    (* ct and ct *)
    (* <:class_type< $ct1$ and $ct2$ >> *)
    (* mutually recursive class types *)
    | CtAnd of loc and class_type and class_type

    (* ct : ct *)
    (* <:class_type< $decl$ : $ctyp:ct$ >> *)
    (* class  c : object .. end   class declaration as in
       "class c: object .. end " in a signature 
    *)
    | CtCol of loc and class_type and class_type

    (* ct = ct *)
    (* <:class_type< $decl$ = $ctyp:ct$ >> *)
    (* class type declaration/definition as in "class type c = object .. end " *)
    | CtEq  of loc and class_type and class_type
      

    (* $s$ *)
    | CtAnt of loc and string
    (**
       <:class_type< $id:i$ [ $list:p$] >>
       --->
       <:class_type< $virtual:Ast.BFalse$ $id:i$ [ $list:p$] >>

       <:class_type< $virtual:v$ $id:i$ >>
       --->
       <:class_type< $virtual:v$ $id:i$ [ $<:ctyp< >>$ ] >>

       <:class_type< object $x$ end >>
       --->
       <:class_type< object ($<:ctyp< >>$) $x$ end >>
    *)
(** CtCon is used for possibly instanciated/parametrized class
    type identifiers. They appear on the left hand side of class
    declaration and class definitions or as reference to existing
    class types. In the latter case the virtual flag is probably
    irrelevant.
    

    Several type parameters/arguments are stored in a TyCom tree, use
    Ast.list_of_ctyp and Ast.tyCom to convert to and from list of
    parameters/arguments.

    An empty type parameter list and an empty type argument is
    represented with the empty type.

    The self binding in class body types is represented by a type
    expression. If the self binding is absent, the empty type
    expression (<:ctype< >>) is used.

    Several class signature items are stored in a CgSem tree, use
    Ast.list_of_class_sig_item and Ast.cgSem_of_list to convert to and
    from a list of class signature items
    
*)
]
  and class_sig_item =
    (**
       class_signature_item, class_sig_item, csg ::=
   | (* empty *)
   | type t = t
   | csg ; csg
   | inherit ct
   | method s : t or method private s : t
   | value (virtual)? (mutable)? s : t
   | method virtual (mutable)? s : t
   | $s$
    *)
    [

      (* <:class_sig_item< >>  *)
      CgNil of loc

    (* <:class_sig_item< constraint $typ:type1$ = $typ:type2$ >>
       type constraint  *)
    | CgCtr of loc and ctyp and ctyp

    (* csg ; csg *)
    | CgSem of loc and class_sig_item and class_sig_item

    (* inherit ct *)
    (* <:classs_sig_item< inherit $ctyp:class_type$ >>  *)
    | CgInh of loc and class_type

    (* method s : t or method private s : t *)
    (* <:class_sig_item< method $private:pf$ $lid:id$:$typ:type$ >>    *)
    | CgMth of loc and string and private_flag and ctyp

    (* value (virtual)? (mutable)? s : t *)
    (* <:class_sig_item< value $mutable:mf$ $virtual:vf$ $lid:id$ : $typ:type$ >>  *)
    | CgVal of loc and string and mutable_flag and virtual_flag and ctyp

    (* method virtual (private)? s : t *)
    (* <:class_sig_item< method virtual $private:pf$ $lid:id$ : $typ:type$ >>   *)
    | CgVir of loc and string and private_flag and ctyp

    (* <:class_sig_item< $anti:a$ >>    *)
    | CgAnt of loc and string (* $s$ *)

    (**
       <:class_sig_item< type $typ:type_1$ = $typ:type_2$
       --->
       <:class_sig_item< constraint $typ:type_1$ = $typ:type_2$ >>

       The empty class signature item is used as a placehodler in
       empty class body types (class type e = object end )
    *)
]
and class_expr =
  (** Ast nodes of this type are additionally used to describe whole
      (mutually recursive) class definitions.


      class_expression, class_expr, ce ::=
   | (* empty *)
   | ce e
   | (virtual)? i ([ t ])?
   | fun p -> ce
   | let (rec)? bi in ce
   | object (p) (cst) end
   | ce : ct
   | ce and ce
   | ce = ce
   | $s$
  *)
  [
    CeNil of loc

    (* ce e *)
    (*
      <:class_expr< $cexp:ce$ $exp:exp$ >>

      application 
    *)
    | CeApp of loc and class_expr and expr

    (* (virtual)? i ([ t ])? *)
    (* <:class_expr< $virtual:vf$ $id:ident$ 
       [ $typ:type_param$ ] >>

       instanciated class/ left hand side of class
       definitions.

       CeCon of Loct.t and vf and ident and type_param
    *)
    | CeCon of loc and virtual_flag and ident and ctyp

    (* fun p -> ce *)
    (* <:class_expr< fun $pat:pattern$ -> $cexp:ce$ >>
       class valued funcion

       CeFun of Loc.t and pattern and ce 
    *)
    | CeFun of loc and patt and class_expr

    (* let (rec)? bi in ce *)
    (* <:class_expr< let $rec:rf$ $binding:binding$ in $cexp:ce$ >>  *)
    | CeLet of loc and rec_flag and binding and class_expr

    (* object ((p))? (cst)? end *)
    (* <:class_expr< object ( $pat:self_binding$ ) $cst:class_str_items$ end >> *)
    | CeStr of loc and patt and class_str_item

    (* ce : ct
       type constraint
       <:class_expr< ($cexp:ce$ : $ctyp:class_type$) >>
    *)
    | CeTyc of loc and class_expr and class_type

    (* ce and ce
       mutually recursive class definitions
    *)
    | CeAnd of loc and class_expr and class_expr

    (**
       <:class_expr< $ci$ = $cexp:ce$ >>
       class definition as in class ci = object .. end 
    *)
    | CeEq  of loc and class_expr and class_expr

    (* $s$ *)
    (** <:class_expr< $anti:s$ >> *)
    | CeAnt of loc and string
    (**
       <:class_expr< $id:id$ [$tp$] >>
       ----> non-virtual class/ instanciated class
       <:class_expr< $virtual:Ast.BFalse$ $id:id$ [$tp$ ] >>

       <:class_expr< $virtual:vf$ $id:id$ >>
       ---->
       <:class_expr< $virtual:vf$ $id:id$ [ $<:ctyp< >>$ ] >>

       <:class_expr< fun $pat:p1$ $pat:p2$ -> $cexp:ce$ >>
       ---->
       <:class_expr< fun $pat:p1$ -> fun $pat:p2$ -> $cexp:ce$ >>

       <:class_expr< let $binding:bi$ in $cexp:ce$ >>
       ---->
       <:class_expr< let $rec:Ast.BFalse$ $binding:bi$ in $cexp:ce$ >>

       <:class_expr< let $rec:Ast.BFalse$ $binding:bi$ in $cexp:ce$ >>
       ---->
       <:class_expr< object ( $<:patt< >>$ ) $cst:cst$ end >>
    *)
(** No type parameters or arguments in an instanciated class
    (CeCon) are represented with the empty type (TyNil).

    Several type parameters or arguments in an instanciated class
    (CeCon) are stored in a TyCom tree. Use Ast.list_of_ctyp and
    Ast.tyCom_of_list convert to and from a list of type parameters.

    There are three common cases for the self binding in a class
    structure: An absent self binding is represented by the
    empty pattern (PaNil).  An identifier (PaId) binds the
    object.  A typed pattern (PaTyc) consisting of an identifier
    and a type variable binds the object and the self type.

    More than one class structure item are stored in a CrSem
    tree. Use Ast.list_of_class_str_item and Ast.crSem_of_list to
    convert to and from a list of class items.
*)
]
and class_str_item =
    (**
       class_structure_item, class_str_item, cst ::=
   | (* empty *)
   | cst ; cst
   | type t = t
   | inherit(!)? ce (as s)?
   | initializer e
   | method(!)? (private)? s : t = e or method (private)? s = e
   | value(!)? (mutable)? s = e
   | method virtual (private)? s : t
   | value virtual (private)? s : t
   | $s$
    *)
  [
    CrNil of loc

    (* cst ; cst *)
    | CrSem of loc and class_str_item and class_str_item

    (* type t = t *)
    (* <:class_str_item< constraint $typ:type_1$ = $typ:type_2$ >>
       type constraint 
    *)
    | CrCtr of loc and ctyp and ctyp

    (* inherit(!)? ce (as s)? *)
    (* <:class_str_item< inherit $!:override$ $cexp:class_cexp$ as $lid:id$ >>  *)
    | CrInh of loc and override_flag and class_expr and string

    (* initializer e *)
    (* <:class_str_item< initializer $exp:expr$ >>  *)
    | CrIni of loc and expr

    (** method(!)? (private)? s : t = e or method(!)? (private)? s = e *)
    (** <:class_str_item< method $!override$ $private:pf$ $lid:id$: $typ:poly_type$ = 
	$exp:expr$ >>
    *)		
    | CrMth of loc and string and override_flag and private_flag and expr and ctyp

    (* value(!)? (mutable)? s = e *)
    (** <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ = $exp:expr$ >>
       instance variable 
    *)
    | CrVal of loc and string and override_flag and mutable_flag and expr

    (** method virtual (private)? s : t *)
    (** <:class_str_item< method virtual $private:pf$ $lid:id$ : $typ:poly_type$ >>
       virtual method 
    *)
    | CrVir of loc and string and private_flag and ctyp

    (* value virtual (mutable)? s : t *)
    (* <:class_str_item< value virtual $mutable:mf$ $lid:id$ : $typ:type$ >> *)
    (* virtual instance variable  *)
    | CrVvr of loc and string and mutable_flag and ctyp

    | CrAnt of loc and string (* $s$ *)

(**
   << constraint $typ:type_1$ = $typ:type_2$ >>
   ----> type constraint 
   << type $typ:type1$ = $typ:type2$ >>

   << inherit $!:override$ $cexp:class_exp$ >>
   ---> superclass without binding
   << inherit $!:override$ $cexp:class_exp$ as $lid:""$ >>

   <<inherit $cexp:class_exp$ as $lid:id$ >>
   ---> superclass without override
   <<inherit $!:Ast.OvNil$ $cexp:class_exp$ as $lid:id$ >>

   <:class_str_item< method $private:pf$ $lid:id$ : $typ:poly_type$ = $exp:expr$ >>
   non-overriding method
   <:class_str_item< method $!:Ast.OvNil$
   $private:pf$ $lid:id$ : $typ:poly_type$ = $exp:expr$ >>
   
   <:class_str_item< method $private:pf$ $lid:id$ = $exp:expr$ >>
   monomorphic method
   <:class_str_item< method $private:pf$ $lid:id$ : $typ:<:ctyp< >>$ = $exp:expr$ >>

   <:class_str_item< method $lid:id$ : $typ:poly_type$ = $exp:expr$ >>
   public method
   <:class_str_item< method $private:Ast.PrNil$ $lid:id$ : $typ:poly_type$ = $exp:expr$ >>

   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ $pat:pattern$ = $exp:expr$ >>
   method arguments
   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ = fun $pat:pattern$ -> $exp:expr$ >>

   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ : $typ:res_type$ = $exp:expr$ >>
   return type constraint
   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ = ($exp:expr$ : $typ:res_type$) >>

   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ :> $typ:res_type$ = $exp:expr$ >>
   return type coercion
   <:class_str_item< method $private:pf$ $lid:id$ :
   $typ:poly_type$ = ($exp:expr$ :> $typ:res_type$ ) >>

   <:class_str_item< value $mutable:mu$ $lid:id$ = $exp:expr$ >>
   non-overriding instance variable
   <:class_str_item< value $!:Ast.OvNil$ $mutable:mf$ $lid:id$ = $exp:expr$ >>

   <:class_str_item< value $!:override$ $lid:id$ = $exp:expr$ >>
   immutable instance variable
   <:class_str_item< value $!:override$ $mutable:Ast.MuNil$ $lid:id$ = $exp:expr$ >>

   <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ :
   $typ:res_type$ = $exp:expr$ >>
   type restriction<:class_str_item
   < value $!:override$ $mutable:mf$ $lid:id$ =
   ($exp:expr$ : $typ:res_type$) >>

   <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ :>
   $typ:res_type$ = $exp:expr$ >>
   simple value coercion
   <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ =
   ($exp:expr$ :> $typ:res_type$) >>

   <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ :
   $typ:expr_type$ :> $typ:res_type$ = $exp:expr$ >>
   complete value coercion
   <:class_str_item< value $!:override$ $mutable:mf$ $lid:id$ =
   ($exp:expr$ : $typ:expr_type$ :> $typ:res_type$) >>

   <:class_str_item< method virtual $lid:id$ : $typ:poly_type$ >>
   public virtual method
   <:class_str_item< method $private:Ast.PrNil$ virtual $lid:id$ : $typ:poly_type$ >>

   <:class_str_item< value $!:override$ virtual $lid:id$ : $typ:type$ >>
   immutable virtual value
   <:class_str_item< value $!:override$ virtual $mutable:Ast.MuNil$ $lid:id$
   : $typ:type$ >>

   A missing superclass binding is represented with the empty string
   as identifier.  Normal methods and explicitly polymophically typed
   methods are represented with the same ast node (CrMth). For a normal
   method the poly_type field holds the empty type (TyNil).
*)
];
