
open Camlp4.PreCast;
open Syntax;

(** Here we know how to use sequence
    we insert at the
    expr:LEVEL "top"

    ipatt??
    
*)


value transform patts expr _loc =
    let rec aux patts =
      match  patts with
	  [ [] -> expr
	  | [h::t] -> <:expr< fun [ $h$ -> $aux t $ ]>>
	  ]
    in aux patts
;

(**
   (fun s -> print_endline s); () or
   fun s -> (print_endline (); ())
*)


(**
  EXTEND Gram
  expr: LEVEL "top"
    [ [ "for"; patts = LIST1 ipatt; "in"; m = a_UIDENT; e = expr;
       "do"; seq = do_sequence ->
         (* ... *)
      | "for"; i = a_LIDENT; "="; e1 = sequence; df = direction_flag;
       e2 = sequence; "do"; seq = do_sequence ->
         (* ... *)
      ] ]
  ;
  END
*)

begin
  DELETE_RULE Gram  expr:
    "for"; LIDENT; "="; sequence;
   direction_flag; sequence; "do"; do_sequence
   END;

   EXTEND Gram
     expr: LEVEL "top"
    [ [ "for"; i = ipatt;
	"=";
	e1 = sequence;
	df = direction_flag;
	e2 = sequence;
	"do";
	seq = do_sequence ->
         <:expr<
           for $lident_of_patt i$ =
             $mksequence _loc e1$ $to:df$ $mksequence _loc e2$
           do { $seq$ }
         >>
      | "for"; p = ipatt; patts = LIST0 ipatt; "in"; m = a_UIDENT; e = expr;
       "do"; seq = do_sequence ->
       let patts = p :: patts in
       let f = mkfun _loc patts seq in
         <:expr< $uid:m$.iter $f$ $e$ >>
      ] ]
  ;
END
    
EXTEND Gram expr:LEVEL "top"
  [
    [
    "for"; patts = LIST1 ipatt; "in"; m=UIDENT;"do"; seq=sequence;
    "done" ->
    <:expr< $uid:m$.iter
      $ transform patts <:expr<do { $seq$ }>> _loc  $ >>
    ]
  ];
 END ;
  
(**
camlp4o -I _build -parser pa_foreach.cmo -str 'for a in List l do print_string "ghsogh"; "xxx"; done '

   List.iter (fun a -> print_string "ghsogh"; "xxx") l
*)

(*
class type ['a] iterable = object
  method iter : ('a -> unit) -> unit 
end 

class ['a] iterable_of_list (lst:'a list) : ['a] iterable  = object
  method iter f =
    List.iter f lst 
end 

let iter1 = new iterable_of_list [1;2;3;4]  

let _ = iter1#iter print_int   

  
class ['a] iterable_of_array (arr : 'a array)
  : ['a] iterable = object
  method iter f =
    Array.iter f arr
end

let iter2 = new iterable_of_array [|1;2;3;4|]
let _ = iter2#iter print_int
*)
