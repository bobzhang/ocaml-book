open Camlp4.PreCast;
open Syntax;

EXTEND Gram expr:
  [ "top" RIGHTA
      [ "let"; r = opt_rec; bi = binding; "in"; x = SELF ->
        <:expr< let $rec:r$ $bi$ in $x$ >>
      | "let"; "module"; m = UIDENT; mb = module_binding0;
	"in";  e = SELF ->
	<:expr< let module $m$ = $mb$ in $e$ >>
	(** allows X.Y.Z *)			      
      | "let"; "open"; i = module_longident; "in"; e = SELF ->
        <:expr< let open $id:i$ in $e$ >>
      | "fun"; "["; a = LIST0 match_case0 SEP "|"; "]" ->
        <:expr< fun [ $list:a$ ] >>
      | "fun"; e = fun_def -> e
      (** ? sequence *)
      | "match"; e = sequence; "with"; a = match_case ->
        <:expr< match $mksequence' _loc e$ with [ $a$ ] >>
      | "try"; e = sequence; "with"; a = match_case ->
        <:expr< try $mksequence' _loc e$ with [ $a$ ]>>
      | "if"; e1 = SELF; "then"; e2=SELF; "else"; e3=SELF ->
        <:expr< if $e1$ then $e2$ else $e3$ >>
      | "do"; seq = do_sequence -> mksequence _loc seq
      | "for"; i = LIDENT; "=";  e1 = sequence; df = direction_flag;
        e2 = sequence;  "do"; seq = do_sequence ->
	  <:expr< for $i$ = $mksequence' _loc e1$
		  $to:df$ $mksequence' _loc e2$
	    do { $seq$ }>>
      | "while"; e = sequence; "do"; seq=do_sequence ->
        <:expr< while $mksequence' _loc e$ do { $seq$ } >>
      | "object"; cps = opt_class_self_patt;
	  cst = class_structure; "end" ->
	  <:expr< object ($cps$) $cst$ end >>
      ]

  | "where"
      [ e = SELF ; "where"; rf = opt_rec ; lb = let_binding ->
      <:expr< let $rec:rf$ $lb$ in $e$ >>
      ]

  | ":=" NONA
      [ e1 = SELF; ":="; e2 = SELF; dummy -> 
	match bigarray_set _loc e1 e2 wiith
	  [ Some e -> e
	  | None -> <:expr< $e1$ := $e2$ >>] ]

  | "||" RIGHTA
      [ e1 = SELF; op = infixop6; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>
      ]

  | "&&" RIGHTA
      [ e1 = SELF; op = infixop5; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>
      ]

  | "<" LEFTA
      [ e1 = SELF; op = infixop0; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>
      ]

  | "^" RIGHTA
      [ e1 = SELF; op = infixop1; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>]

  | "+" LEFTA
      [ e1 = SELF; op = infixop2; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>
      ]

  | "*" LEFTA
      [ e1 = SELF; "land"; e2 = SELF -> <:expr< $e1$ land $e2$ >>
      | e1 = SELF; "lor";  e2 = SELF -> <:expr< $e1$ lor $e2$ >>
      | e1 = SELF; "lxor"; e2 = SELF -> <:expr< $e1$ lxor $e2$ >>
      | e1 = SELF; "mod";  e2 = SELF -> <:expr< $e1$ mod $e2$ >>
      | e1 = SELF; op = infixop3; e2 = SELF ->
        <:expr< $op$ $e1$ $e2$ >>
      ]

  | "**" RIGHTA
      [ e1 = SELF; "asr"; e2 = SELF ->  <:expr< $e1$ asr $e2$ >>
      | e1 = SELF; "lsl"; e2 = SELF ->  <:expr< $e1$ lsl $e2$>>
      | e1 = SELF; "lsr"; e2 = SELF ->  <:expr< $e1$ lsr $e2$ >>
      | e1 = SELF; op = infixop4; e2 = SELF ->
      <:expr< $op$ $e1$ $e2$ >>]

  (** Higher priority *)    
  | "unary minus" NONA
      [ "-"; e = SELF -> mkumin _loc "-" e
      | "-."; e = SELF -> mkumin _loc "-." e ]

  | "apply" LEFTA
      [ e1 = SELF; e2 = SELF -> <:expr< $e1$ $e2$ >>
      | "assert"; e = SELF -> mkassert _loc e
      | "new"; i = class_longident -> <:expr< new $i$ >>
      | "lazy"; e = SELF -> <:expr< lazy $e$ >> ]

  | "label" NONA
      [ "~"; i = LIDENT; ":"; e = SELF ->
      <:expr< ~$i$ : $e$ >>
      | "~"; i = LIDENT ->
      <:expr< ~$i$>>
      (** ?? *)
      | `LABEL i; e = SELF ->
      <:expr< ~$i$ : $e$ >>
      | `OPTLABEL i ; e = SELF ->
      <:expr< ? $i$ : $e$ >>
      | "?"; i = LIDENT; ":"; e = SELF ->
      <:expr< ? $i$ : $e$ >>
      | "?"; i = LIDENT -> <:expr< ? $i$ >>
      ]

  | "." LEFTA
      [ e1 = SELF ; "."; "("; e2 = SELF; ")" ->
      <:expr< $e1$.($e2$) >>
      | e1 = SELF; "."; "["; e2 = SELF; "]" ->
      <:expr< $e1$.[ $e2$ ]>>
      | e1 = SELF; "."; "{"; e2 = comma_expr; "}" ->
      bigarray_get _loc e1 e2
      | e1 = SELF; "."; e2 = SELF ->
      <:expr< $e1$ . $e2$ >>
      | e = SELF; "#"; lab = label ->
      <:expr< $e$ # $lab$ >>
      ]

  | "~-" NONA
      ["!"; e = SELF ->
      <:expr< $e$.val >>
      | f = prefixop; e = SELF ->
      <:expr< $f$ $e$ >>
      ]

  | "simple"
      [`QUOTATION x ->
	Quotation.expand _loc x
	  Quotation.DynAst.expr_tag
      |`ANTIQUOT ("exp" | "" | "anti" as n) s ->
	<:expr< $anti:mk_anti ~c:"expr" n s $ >>
      (** camlp4of -str '<:expr< $`bool: True $ >>' Ast.ExId (_loc,
	  (Ast.IdUid (_loc, (if True then "True" else "False")))) *)
      |`ANTIQUOT ("`bool" as n) s ->
	<:expr< $id: <:ident< $anti:mk_anti n s $ >> $ >>
      |`ANTIQUOT ("tup" as n ) s ->
	<:expr< $tup: <:expr< $anti:mk_anti ~c:"expr" n s $ >> $ >>
      |`ANTIQUOT ("seq" as n ) s ->
	<:expr< do $anti:mk_anti ~c:"expr" n  s $ done >>
      | s = INT -> <:expr< $int:s$ >>
      | s = INT32 -> <:expr< $int32:s$ >>
      | s = INT64 -> <:expr< $int64:s$>>
      | s = NATIVEINT -> <:expr< $nativeint:s$ >>
      | s = FLOAT -> <:expr< $flo:s$ >>
      | s = CHAR -> <:expr< $chr:s$ >>
      (** new syntax since 3.12 *)
      | i = TRY module_longident_dot_lparen; e = sequence; ")" ->
        <:expr< let open $i$ in $e$ >>
      | i = TRY val_longident -> <:expr< $id:i$ >>
      | "`"; s = ident -> <:expr< ` $s$ >>
      | "[";"]" -> <:expr< [] >>
      | "["; mk_list = sem_expr_for_list; "::";
	last = expr; "]" ->
	mk_list last
      | "["; mk_list = sem_expr_for_list; "]" -> 
        mk_list <:expr< [] >>
      | "[|";  "|]" -> <:expr< [| $<:expr< >>$ |] >>
      | "[|";  el = sem_expr; "|]" ->
        <:expr< [| $el$ |] >>
      | "{"; el = label_expr_list ; "}" ->
        <:expr< { $el$ } >>
      | "{"; "("; e = SELF; ")"; "with"; el = label_expr_list; "}" ->
        <:expr< { ($e$) with $el$ } >>
      | "("; ")" -> <:expr< () >>
      | "("; e = SELF; ":"; t = ctyp; ")" ->
        <:expr< ( $e$ : $t$ ) >>
      | "("; e = SELF; ","; e1 = comma_expr; ")" ->
        <:expr< ( $e$, $el$ ) >>
      | "("; e = SELF; ";" seq = sequence; ")" ->
        mksequence _loc <:expr< $e$ ; $seq$ >>
      | "("; e = SELF; ";"; ")" -> mksequence _loc e
      | "("; e = SELF; ":";  t = ctyp; ":>"; t2 = ctyp; ")" ->
        <:expr< ( $e$ : $t$ :> $t2$ ) >>
      | "("; e = SELF; ":>"; t = ctyp; ")" ->
        <:expr< ( $e$ :> $t$ ) >>
      | "("; e = SELF; ")" -> e
      | "begin"; seq = sequence; "end" -> mksequence _loc seq
      | "begin"; "end" -> <:expr< () >>
      (** first class module *)
      | "("; "module"; me = module_expr; ")" ->
        <:expr< (module $me$ )>>
      | "("; "module"; me = module_expr; ":"; pt = package_type; ")" ->
        <:expr< (module $me$ : $pt$ ) >>
      ]
  ]
 ;
  (**
     All are trying, if it failed , it will not consume any
  *)
  do_sequence:
    [
      [ seq = TRY ["{"; seq =sequence; "}" -> seq ] ->	seq
      | TRY ["{"; "}" ] -> <:expr< () >>
      | seq = TRY [seq=sequence; "done" -> seq] -> seq
      | "done" ->  <:expr< () >>
      ]
    ]
  ;
  infixop5:
    [[ x = ["&"; "&&"] -> <:expr< $lid:x$ >> ] ]
  ;
  infixop6:
    [[ x = ["or"; "||"] -> <:expr< $lid:x$ >> ] ]
  ;
  (** interesing case *)
  sem_expr_for_list:
    [
      [ e = expr; ";"; el = SELF ->
        fun acc ->  <:expr<  [ $e$ :: $el acc $ ] >>
      | e = expr; ";" ->
        fun acc -> <:expr< [ $e$ :: $acc$ ] >>
      | e = expr ->
	fun acc -> <:expr< [ $e$ :: $acc$ ] >>
      ]
    ]
  ;
  comma_expr:
    [ [
      e1 = SELF; ","; e2 = SELF ->
      <:expr< $e1$, $e2$ >>
    (** interesting case parse cases like this (a,$ls$ ) *)
    | `ANTIQUOT("list" as n) s ->
      <:expr< $anti:mk_anti ~c:"expr," n s $ >>
    (** ??? *)	
    | e = expr LEVEL "top" -> e  ] ]
  ;
  (** ?? *)
  dummy:
    [ [ -> () ] ]
    ;
  
  sequence': [[  -> fun e -> e
	      | ";" -> fun e -> e
	      | ";"; el = sequence ->
	      fun e -> <:expr< $e$ ; $el$ >> ] ]
  ;
  sequence:
    [[ "let"; rf = opt_rec; bi = binding;"in";
       e = expr; k = sequence'
     -> k <:expr< let $rec:rf$ $bi$ in $e$ >>

     (** imperative style *)				    
     | "let"; rf = opt_rec; bi = binding; ";";
       el = SELF
     -> <:expr< let $rec:rf$ $bi$ in $mksequence _loc el $ >>
     | "let"; "module"; m = UIDENT; mb = module_binding0;
	"in"; e = expr; k = sequence' ->
	k <:expr< let module $m$ = $mb$ in $e$ >>
     (** imperative style *)					
     | "let"; "module"; m = UIDENT; mb = module_binding0;
	";"; el = SELF ->
	<:expr< let module $m$ = $mb$ in $mksequence _loc el$ >>
     | "let"; "open"; i = module_longident; "in"; e = SELF ->
       <:expr< let open $id:i$ in $e$ >>
     | `ANTIQUOT("list" as n) s ->
       <:expr< $anti:mk_anti ~c:"expr;" n s $>>
     | e = expr; k = sequence' -> k e 
     ]
    ]
  ;
  binding:
    [ LEFTA
	[ `ANTIQUOT ("binding"|"list" as n) s ->
	<:binding< $anti:mk_anti ~c:"binding" n s >>
	| `ANTIQUOT (""|"anti" as n ) s; "="; e = expr ->
	<:binding< $anti:mk_anti ~c:"patt" n s$ = $e$ >>
	| `ANTIQUOT (""|"anti" as n) s ->
	  <:binding< $anti:mk_anti ~c:"binding" n s $ >>
	| b1 = SELF; "and"; b2 = SELF ->
	  <:binding< $b1$ and $b2$ >>
        | b = let_binding -> b 
	]
    ]
  ;

   (** binding is delegated to ipatt *)
   let_binding:
     [ [ p = ipatt; e = fun_binding -> <:binding< $p$ = $e$ >>] ]
   ;
   fun_bindings:
     [RIGHTA
       [ TRY ["("; "type" ]; i = LIDENT; ")"; e=SELF ->
         <:expr< fun (type $i$) -> $e$ >>
       | p = TRY labeled_ipatt; e = SELF ->
         <:expr< fun $p$ -> $e$ >>
       | bi = cvalue_binding -> bi 
       ]
     ]
   ;
   match_case:
     [
       [
       "["; l = LIST0 match_case0 SEP "|"; "]" ->
       Ast.mcOr_of_list l
       | p = ipatt ; "->"; e = expr ->
       <:match_case< $p$ -> $e$ >>
       ]
     ];
   (** To be finished *)
   match_case0:
     [
       [
       ]
     ]
   ;
  END;
