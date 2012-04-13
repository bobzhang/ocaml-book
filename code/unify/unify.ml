(* Author: bobzhang1988@vpl342.wlan.library.upenn.edu        *)
(* Version: $Id: unify.ml,v 0.0 2012/02/16 01:19:23 bobzhang1988 Exp $ *)

open Printf
open Util
open Sexplib.Std
(** constant is also viewed as term with varity 0  *)
type ('a,'b) term = 
  | Term of 'a * ('a,'b) term list 
  | Var of 'b
with sexp 

let print_term (t:(string,string) term) =
  print_string ^$
    string_of_sexp ^$
    sexp_of_term sexp_of_string sexp_of_string t
    
open Camlp4.PreCast

type ('a,'b) delta =
    ('b *  ('a,'b) term) list
with sexp 

(** The main algorithm guarantees x does not occur in delta *)
let rec robOccursCheck  (x:'a) (ts: ('b,'a) term list)
    (delta: ('b,'a) delta)  =
  match ts with
    | [] -> true
    | h::t ->
      (match h with
	| Var v ->
	  if x = v
	  then false
	  else
	    (try
	       robOccursCheck x [ (List.assoc v delta) ] delta
	     with
		 Not_found -> true )
	| Term (f, terms) ->
	  robOccursCheck x terms delta  )
      && robOccursCheck x t delta


(** substitution until x is not a variable bound by delta *)	
let subst_r (x:('a,'b)term) (delta:('a,'b) delta) : ('a,'b)term =
  let cur = ref x in 
  (try 
     while true  do
       match !cur with
	 | Var x ->
	   (match List.assoc x delta with
	     | Var y as res ->
	       cur := res
	     | Term _ as res ->
	       cur := res ; raise Not_found (*end*)
	   )
	 | Term _  ->
	   raise Not_found
     done
   with Not_found -> ())
  ; !cur

let rec subst (x,s)  t =
  match t with
    | Var x'->
      if  x = x'
      then  s
      else t 
    | Term (f,ls) ->
      Term(f, List.map (subst (x,s)) ls )

(* let rec re_supp xs = *)
(*   match xs with  *)
(*     | [] -> [] *)
(*     | h::t -> *)
(*       let res = re_supp t in  *)
(*       h:: List.map (fun (k,v) -> k, subst h v) res *) 

	
let rob (sts : (('a,'b) term * ('a,'b) term) list) : ('a,'b) delta option  =
  let rec aux sts acc : ('a,'b) delta =
    (match sts  with
      | [] ->  acc
      | (ox,oy) :: t ->
	let (x,y) = (subst_r ox acc, subst_r oy acc) in
	(if x = y
	 then aux t acc
	 else 
 	    (match x,y with
	      | Var x', Var y' ->
		aux t ( (x', y) :: List.map (fun (k,v) -> k,  subst (x',y)  v )  acc)
	      | Var x', Term _ ->
		if robOccursCheck x' [y] acc
		then aux t ((x', y) :: List.map (fun (k,v) -> k , subst (x',y) v ) acc)
		else raise Not_found
	      | Term _ , Var y' ->
		if robOccursCheck y' [x] acc
		then aux t ((y',x) :: List.map (fun (k,v) ->  k, subst (y',x) v) acc)
		else raise Not_found
	      | Term (f,fs), Term(g,gs) ->
		if f = g && List.length fs = List.length gs then
		  aux t (aux (List.combine fs gs) acc)
		else raise Not_found ))) in 
  try
    let res  = aux sts [] in
    Some res
    (* Some (re_supp res ) *)
  with Not_found -> None




module MGram = MakeGram(Lexer)
let term, term_eoi = MGram.Entry.mk "term", MGram.Entry.mk "term_eoi"
let pair = MGram.Entry.mk "pair"

let _ = begin
  MGram.Entry.clear term;
  MGram.Entry.clear term_eoi;
  MGram.Entry.clear pair;
  EXTEND MGram GLOBAL: term term_eoi pair;
  term_eoi:
  [[ t = term ; EOI -> t  ]]
  ;
  term:
  [[ f = LIDENT ; "("; terms = LIST0 SELF SEP ",";  ")" -> 
  Term(f, terms)
   | f = LIDENT -> Term (f,[])
   | v = UIDENT -> Var v  ]]
  ;
  pair:
    [[ ls = LIST1  [l = term; "=" ; r = term -> (l,r)] SEP "," ; EOI->
       rob ls
    ]]
  ;
  END;
end

let term_of_string = MGram.parse_string term_eoi (Loc.mk "<string>")
let pair_of_string = MGram.parse_string pair (Loc.mk "<pair>")  

