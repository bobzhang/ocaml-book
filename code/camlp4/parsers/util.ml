
open Camlp4.PreCast.Syntax;
open Camlp4.PreCast;
open Camlp4.PreCast.Ast;
open Camlp4.Sig;


(**
   [neg_string "ab" ] = ["-ab"]
   [neg_string ""] = ["-"]
 *)
value neg_string n =
  let len = String.length n in
  if len > 0 && n.[0] = '-' then String.sub n 1 (len - 1)
  else "-" ^ n
;

(**
      | "unary minus" NONA
      [ "-"; e = SELF -> mkumin _loc "-" e
      | "-."; e = SELF -> mkumin _loc "-." e ]

   since ocaml respect (~-) as a prefix (-)
   and (~-.) as a prefix (-.)
 *)
value mkumin _loc f arg =
  match arg with
  [ <:expr< $int:n$ >> -> <:expr< $int:neg_string n$ >>
  | <:expr< $int32:n$ >> -> <:expr< $int32:neg_string n$ >>
  | <:expr< $int64:n$ >> -> <:expr< $int64:neg_string n$ >>
  | <:expr< $nativeint:n$ >> -> <:expr< $nativeint:neg_string n$ >>
  | <:expr< $flo:n$ >> -> <:expr< $flo:neg_string n$ >>
  | _ -> <:expr< $lid:"~" ^ f$ $arg$ >> ]
;

(**
   given a list expr, translate it into a expr
   Here [Loc.merge (Ast.loc_of_expr e1) _loc ] will
   give more precise location
   [mklistexp : loc -> option expr -> list expr -> expr ]

   
   [mklistexp _loc None [ <:expr<3 >> ; <:expr<4 >> ] |> opr#expr std_formatter;

   =>

   [ 3; 4 ]
 *)
value mklistexp _loc last =
  loop True where rec loop top =
    fun
    [ [] ->
      match last with
      [ Some e -> e
      | None -> <:expr< [] >> ]
    | [e1 :: el] ->
       let _loc =
          if top then _loc else Loc.merge (Ast.loc_of_expr e1) _loc
       in
       <:expr< [$e1$ :: $loop False el$] >> ]
;

(**
   Camlp4 treats [assert False] as a special construct [ExAsf]

   # <:expr< assert False>>;
   <:expr< assert False>>;
   - : Camlp4.PreCast.Ast.expr = ExAsf 
   # <:expr< assert $ <:expr< False >> $>>;
   <:expr< assert $ <:expr< False >> $>>;
   - : Camlp4.PreCast.Ast.expr = ExAsr  (ExId  (IdUid  "False"))
 *)      
value mkassert _loc =
  fun
  [ <:expr< False >> ->
    <:expr< assert False >> (* this case takes care about
                                   the special assert false node *)
    | e -> <:expr< assert $e$ >> ]
  ;

value append_eLem el e = el @ [e];

(**
   mk_anti ~c:"binding" "list" "code" ;
   - : string = "\\$listbinding:code"
 *)    
value mk_anti ?(c = "") n s = "\\$"^n^c^":"^s;

(**

   This is translated as
   value mksequence _loc =
     fun [ (Ast.ExSem _ _ _ | Ast.ExAnt _ _ as e) -> Ast.ExSeq _loc e | e -> e ];

   Notice [ <:expr< $_$ >> ] would be translated to underscore directly
   in most case ExAnt is embedded as a hole in a bigger ast.

    708:        | "("; e = SELF; ";"; ")" -> mksequence _loc e
    713:        | "begin"; seq = sequence; "end" -> mksequence _loc seq
    757:            <:expr< let $rec:rf$ $bi$ in $mksequence _loc el$ >>
    761:            <:expr< let module $m$ = $mb$ in $mksequence _loc el$ >>

 *)
value mksequence _loc =
    fun
    [ <:expr< $_$; $_$ >> | <:expr< $anti:_$ >> as e -> <:expr< do { $e$ } >>
    | e -> e ]
  ;


(**
    600:            <:expr< match $mksequence' _loc e$ with [ $a$ ] >>
    602:            <:expr< try $mksequence' _loc e$ with [ $a$ ] >>
    608:            <:expr< for $i$ = $mksequence' _loc e1$ $to:df$ $mksequence' _loc e2$ do { $seq$ } >>
    610:            <:expr< while $mksequence' _loc e$ do { $seq$ } >>
   
 *)      
value mksequence' _loc =
    fun
    [ <:expr< $_$; $_$ >> as e -> <:expr< do { $e$ } >>
    | e -> e ]
  ;


(**
   lid_of_ident <:ident< A.B.c >>;
   - : string = "c"
   
 *)      
value rec lid_of_ident =
    fun
    [ <:ident< $_$ . $i$ >> -> lid_of_ident i
    | <:ident< $lid:lid$ >> -> lid
    | _                     -> assert False ];


(**
module_type_app <:module_type< A >> <:module_type< B >>;
- : Camlp4.PreCast.Ast.module_type = MtId  (IdApp  (IdUid  "A") (IdUid  "B"))

<:module_type< A >>;
- : Camlp4.PreCast.Ast.module_type = MtId  (IdUid  "A")
   Here we need define module_type_app, since
     |	IdApp of loc * ident* ident
     |	ExApp of loc * expr * expr
   but for module_expr
     |	MeId of loc * ident
     |	MeApp of loc * module_expr * module_expr

   since we require that for module_type_app operation, only
   MeId can be used as app operation.
*)      
value module_type_app mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1$ >>, <:module_type< $id:i2$ >>) ->
        <:module_type< $id:<:ident< $i1$ $i2$ >>$ >>
    | _ -> raise Stream.Failure ];

      
(**
module_type_acc <:module_type< A >> <:module_type< B >>;
- : Camlp4.PreCast.Ast.module_type = MtId  (IdAcc  (IdUid  "A") (IdUid  "B"))
 *)      
value module_type_acc mt1 mt2 =
    match (mt1, mt2) with
    [ (<:module_type@_loc< $id:i1$ >>, <:module_type< $id:i2$ >>) ->
        <:module_type< $id:<:ident< $i1$.$i2$ >>$ >>
    | _ -> raise Stream.Failure ];

(**
   | e1 = SELF; "."; "{"; e2 = comma_expr; "}" -> bigarray_get _loc e1 e2
   support bigarray syntax extension

   <:expr<a.{ (b,c)}>>;
- : Camlp4.PreCast.Ast.expr =
ExApp 
 (ExApp 
   (ExApp 
     (ExId 
       (IdAcc  (IdUid  "Bigarray") (IdAcc  (IdUid  "Array2") (IdLid  "get"))))
     (ExId  (IdLid  "a")))
   (ExId  (IdLid  "b")))
 (ExId  (IdLid  "c"))

   remember [Ast.list_of_expr] only handles case (ExCom, ExSem)
   exSem_of_list and exCom_of_list are on the other hand 
 *)
      
      
value bigarray_get _loc arr arg =
    let coords =
      match arg with
      [ <:expr< ($e1$, $e2$) >> | <:expr< $e1$, $e2$ >> ->
          Ast.list_of_expr e1 (Ast.list_of_expr e2 [])
      | _ -> [arg] ]
    in
    match coords with
    [ [c1] -> <:expr< Bigarray.Array1.get $arr$ $c1$ >>
    | [c1; c2] -> <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >>
    | [c1; c2; c3] -> <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >>
    (* | coords -> <:expr< Bigarray.Genarray.get $arr$ [| $list:coords$ |] >> ] *)
    | coords ->
       <:expr< Bigarray.Genarray.get $arr$ [| $Ast.exSem_of_list coords$ |] >> ];

(**
       | ":=" NONA
        [ e1 = SELF; ":="; e2 = SELF; dummy ->
            match bigarray_set _loc e1 e2 with
            [ Some e -> e
            | None -> <:expr< $e1$ := $e2$ >> ] ]
 *)      
value bigarray_set _loc var newval =
    match var with
    [ <:expr< Bigarray.Array1.get $arr$ $c1$ >> ->
        Some <:expr< Bigarray.Array1.set $arr$ $c1$ $newval$ >>
    | <:expr< Bigarray.Array2.get $arr$ $c1$ $c2$ >> ->
        Some <:expr< Bigarray.Array2.set $arr$ $c1$ $c2$ $newval$ >>
    | <:expr< Bigarray.Array3.get $arr$ $c1$ $c2$ $c3$ >> ->
        Some <:expr< Bigarray.Array3.set $arr$ $c1$ $c2$ $c3$ $newval$ >>
    | <:expr< Bigarray.Genarray.get $arr$ [| $coords$ |] >> ->
        Some <:expr< Bigarray.Genarray.set $arr$ [| $coords$ |] $newval$ >>
    | _ -> None ];

(**
     [ [ "#"; n = a_LIDENT; dp = opt_expr; semi ->
       ([ <:sig_item< # $n$ $dp$ >> ], stopped_at _loc)
*)      
value stopped_at _loc =
    Some (Loc.move_line 1 _loc) (* FIXME be more precise *);

(**
symbolchar "a!$" 0;
- : bool = False
# symbolchar "a!$" 1;
symbolchar "a!$" 1;
- : bool = True
 *)
value symbolchar =
    let list =
      ['$'; '!'; '%'; '&'; '*'; '+'; '-'; '.'; '/'; ':'; '<'; '='; '>'; '?';
       '@'; '^'; '|'; '~'; '\\']
    in
    let rec loop s i =
      if i == String.length s then True
      else if List.mem s.[i] list then loop s (i + 1)
      else False
    in
    loop
  ;
(**
   notice that it patterns match KEYWORD, and SYMBOl
 *)
value setup_op_parser entry p =
    Gram.Entry.setup_parser entry
      (parser
        [: `(KEYWORD x | SYMBOL x, ti) when p x :] ->
          let _loc = Gram.token_location ti in
          <:expr< $lid:x$ >>);

(**
   setup prefixop
   Gram.parse_string Syntax.prefixop _loc "!-";
- : Camlp4.PreCast.Syntax.Ast.expr = ExId  (IdLid  "!-")

 *)    
let list = ['!'; '?'; '~'] in
let excl = ["!="; "??"] in
  setup_op_parser prefixop
  (fun x -> not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list && symbolchar x 1);
(**
   setup infixop0 parser
 *)
let list_ok = ["<"; ">"; "<="; ">="; "="; "<>"; "=="; "!="; "$"] in
let list_first_char_ok = ['='; '<'; '>'; '|'; '&'; '$'; '!'] in
let excl = ["<-"; "||"; "&&"] in
  setup_op_parser infixop0
    (fun x -> (List.mem x list_ok) ||
              (not (List.mem x excl) && String.length x >= 2 &&
              List.mem x.[0] list_first_char_ok && symbolchar x 1));

(**
   setup infixop1 parser
 *)
let list = ['@'; '^'] in
  setup_op_parser infixop1
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);
(** setup infixop2 parser *)
let list = ['+'; '-'] in
  setup_op_parser infixop2
    (fun x -> x <> "->" && String.length x >= 1 && List.mem x.[0] list &&
              symbolchar x 1);

(** setup infixop3 parser *)
let list = ['*'; '/'; '%'; '\\'] in
  setup_op_parser infixop3
    (fun x -> String.length x >= 1 && List.mem x.[0] list &&
              (x.[0] <> '*' || String.length x < 2 || x.[1] <> '*') &&
              symbolchar x 1);

(** setup infixop4 parser *)
setup_op_parser infixop4
    (fun x -> String.length x >= 2 && x.[0] == '*' && x.[1] == '*' &&
              symbolchar x 2);
(**
   filter the token stream
   merge `KEYWORD (, `KEYWORD lxor, `KEYWORD )
   into a (LIDENT lxor)

   module Lexer = Camlp4.Struct.Lexer.Make Token;
   value s= Lexer.from_string _loc "(mod)";

   Here we use our brand new Lexer, so we get LIDENT "mod"
   instead of [ KEYWORD "mod"]
   Stream.next s ;
   (** SYMBOL "(",  , LIDENT "mod", SYMBOL ")" *)

 *)
value rec infix_kwds_filter =
    parser
    [ [: `((KEYWORD "(", _) as tok); xs :] ->
        match xs with parser
        [ [: `(KEYWORD ("mod"|"land"|"lor"|"lxor"|"lsl"|"lsr"|"asr" as i), _loc);
             `(KEYWORD ")", _); xs :] ->
                [: `(LIDENT i, _loc); infix_kwds_filter xs :]
        | [: xs :] ->
                [: `tok; infix_kwds_filter xs :] ]
    | [: `x; xs :] -> [: `x; infix_kwds_filter xs :] ]
;

(** introduce filter

    [define_filter]:
     allows to register a new filter to the token filter *chain*.
    value define_filter : t -> (token_filter -> token_filter) -> unit;
 *)        
Token.Filter.define_filter (Gram.get_filter ())
    (fun f strm -> infix_kwds_filter (f strm));

(**
   it's built on top of expr
   given expr, we get symb parser, then
   use parser symb to generate sem_expr.

   If we don't use [parse_tokens_after_filter], what will happen???
   We need to support antiquotations here

   Gram.parse_string Syntax.sem_expr _loc "a;b $list:bbb$";

   - : Camlp4.PreCast.Syntax.Ast.expr =
   ExSem  (ExSem  (ExId  (IdLid  "a")) (ExId  (IdLid  "b")))
      (ExAnt  "\\$listexpr;:bbb")
   (** notice here we get \\$listexpr;:bbb as expected *)
   Gram.parse_string Syntax.sem_expr _loc "a;b $bb$";
   - : Camlp4.PreCast.Syntax.Ast.expr =
   ExSem  (ExId  (IdLid  "a"))
     (ExApp  (ExId  (IdLid  "b")) (ExAnt  "\\$expr:bb"))

   setup_parser translate (stream token -> expr) to (entry expr)
 *)        
Gram.Entry.setup_parser sem_expr begin
    let symb1 = Gram.parse_tokens_after_filter expr in
    let symb =
      parser
      [ [: `(ANTIQUOT ("list" as n) s, ti) :] ->
        let _loc = Gram.token_location ti in
        <:expr< $anti:mk_anti ~c:"expr;" n s$ >>
      | [: a = symb1 :] -> a ]
    in
    let rec kont al =
      parser
      [ [: `(KEYWORD ";", _); a = symb; s :] ->
        let _loc = Loc.merge (Ast.loc_of_expr al)
                             (Ast.loc_of_expr a) in
        kont <:expr< $al$; $a$ >> s
      | [: :] -> al ]
    in
    parser [: a = symb; s :] -> kont a s
  end;
    
  



















