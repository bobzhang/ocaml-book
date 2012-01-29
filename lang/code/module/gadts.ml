(*

                       GADTs in OCaml

  We illustrate one simple, pure, magic-free implementation of a form of GADTs
  in OCaml, and encode some common examples: typed printf/scanf and an
  evaluator for a typed object language.

  GADTs require three components beyond Standard ML provides:

  1. Proofs of equalities between types.  See

        Foundations for Structured Programming with GADTs
        Patricia Johann and Neil Ghani
        POPL 2008

     which reduces every GADT to the equality GADT plus existential
     quantification.

     It is possible to encode a weak form of equality proofs as an
     isomorphism, but first class modules make a full implementation possible.
     See leibniz.ml for the details.

  2. Existential types.  Again, we have a choice of encodings: we can use
     polymorphic record fields or object methods and the well-known encoding
     of existentials using universals (polymorphism).  Alternatively, we can
     make use of the connection between abstract types and existential types,
     and use abstract type components of first class modules.

     First-class modules offer additional power over the polymorphism
     encoding, but we don't need that additional power here.  See
     existentials.ml for the details.

  3. Polymorphic recursion.  Here we're spoilt for choice: there are many
     encodings of polymophirc recursion to choose from in OCaml.  OCaml 3.12
     supports polymorphic recursion directly, which we use below.  We surmise
     that the encoding of polymorphic recursion using first class modules
     once again offers additional power over the alternatives.  Ses
     polymorphic_recursion.ml for the details.

  The GADT notation turns out lightweight, although packing and unpacking
  existentials adds some overhead.  Smart constructors -- which can be build
  mechanically, perhaps with a suitable camlp4 macro -- ease the pain.

  The following implementation is genuinely safe, meaning it never leads
  to segmentation faults, even in principle.
*)

(* We'll use the encoding of equality proofs based on first class
   modules from leibniz.ml *)
open Leibniz

(* ------------------------------------------------------------------------ *)
(* Example 1: typed printf and scanf sharing the same format descriptor

   This is straightforward re-implementation of Haskell code:
   http://okmij.org/ftp/typed-formatting/PrintScanI.txt
   http://okmij.org/ftp/typed-formatting/PrintScan.hs

The Haskell code supports an additional format descriptor
PrintScan.hs to allow printing and scanning of any value that can be
printed and read. We elide this extensibility here for clarity of the example.
*)

(* Please refer to Haskell code for comparison and explanation *)

type ('a,'b) fmt =
  | FLit of ('a, 'b) eq * string
  | FInt of (int -> 'a,'b) eq
  | FChr of (char -> 'a, 'b) eq
  | FCmp of < m_fcmp : 'w. ('a,'b,'w) fcmp_k -> 'w >
      (* The standard encoding of existentials *)
and ('a,'c,'w) fcmp_k =
    {fcmp_k : 'b. ('b,'c) fmt * ('a,'b) fmt -> 'w}


(* Smart constructors, as syntactic sugar.
   Method signatures are, alas, required.

   It should be possible to generate these smart constructors could be
   mechanically using camlp4.
*)
let f_lit x
    = FLit (refl (), x)
(* val f_lit : string -> ('a, 'a) fmt *)

let f_int () = FInt (refl ())
(* val f_int : unit -> ('a, int -> 'a) fmt *)

let f_char () = FChr (refl ())
(* val f_char : unit -> ('a, char -> 'a) fmt *)

let (^^) f1 f2 = FCmp (object method m_fcmp : 'w. ('a,'b,'w) fcmp_k -> 'w
   = fun k -> k.fcmp_k (f1,f2) end)
(* val ( ^^ ) : ('a, 'b) fmt -> ('c, 'a) fmt -> ('c, 'b) fmt *)

(* Two interpreters, for printf and scanf.
   We need polymorphic recursion -- as is typical with GADTs *)
let rec printer : 'a 'b. ('a,'b) fmt -> (string -> 'a) -> 'b =
  function
    | FLit (eq, x) -> fun k -> cast eq (k x)
    | FInt eq -> fun k -> cast eq (fun x -> k (string_of_int x))
    | FChr eq -> fun k -> cast eq (fun x -> k (String.make 1 x))
    | FCmp x -> fun k ->
        x#m_fcmp {fcmp_k =
            fun (a,b) -> printer a (fun sa ->
                         printer b (fun sb -> k (sa ^ sb)))}

let sprintf fmt = printer fmt (fun x -> x)
(* val sprintf : (string, 'a) fmt -> 'a *)

exception Scan_error of string
(* primitive parsers: return the parsed value and the remainder of inp *)
let prefixp str = fun inp ->
  if String.length str <= String.length inp &&
     str = String.sub inp 0 (String.length str)
  then String.sub inp (String.length str)
      (String.length inp - String.length str)
  else raise (Scan_error "lit")

(* val intS : string -> int * string *)
let intS = fun inp ->
  let n = String.length inp in
  let rec loop acc i =
    if i >= n then (acc,"") else
    let c = inp.[i] in
    if c >= '0' && c <= '9' then
      loop (acc * 10 + (int_of_char c - int_of_char '0')) (succ i)
    else if i = 0 then raise (Scan_error "int")
    else (acc, String.sub inp i (n-i)) in
  if n = 0 then raise (Scan_error "int")
  else loop 0 0

(* val charS : string -> char * string *)
let charS = fun inp ->
  if String.length inp = 0
  then raise (Scan_error "char")
  else (inp.[0], String.sub inp 1 (String.length inp - 1))

let rec scanner : 'a 'b. ('a,'b) fmt -> string -> 'b -> 'a * string =
 function
   | FLit (eq, str) -> fun inp consumer -> cast (symm eq) consumer, prefixp str inp
   | FInt eq -> fun inp consumer ->
       let (v,rest) = intS inp in
         (cast (symm eq) consumer v, rest)
  | FChr eq -> fun inp consumer ->
       let (v,rest) = charS inp in
         (cast (symm eq) consumer v, rest)
   | FCmp x -> fun inp consumer ->
       x#m_fcmp {fcmp_k =
          fun (a,b) ->
            let (va,ra) = scanner a inp consumer in
              scanner b ra va}

let sscanf inp fmt f = scanner fmt inp f
(* val sscanf : string -> ('a, 'b) fmt -> 'b -> 'a * string *)

(* Examples *)

let tp1 = sprintf (f_lit "Hello world")
(* val tp1 : string = "Hello world" *)

let ts1 = sscanf tp1 (f_lit "Hello world") ()
(* val ts1 : unit * string = ((), "") *)

let tp2 = sprintf (f_lit "Hello " ^^ f_lit "world" ^^ f_char ()) '!'
(* val tp2 : string = "Hello world!" *)

let ts2 = sscanf tp2 (f_lit "Hello " ^^ f_lit "world" ^^ f_char ()) (fun x -> x)
(* val ts2 : char * string = ('!', "") *)

(* Formats are first-class and can be constructed incrementally *)
let fmt31 () = f_lit "The value of " ^^ f_char () ^^ f_lit " is "
(* val fmt31 : unit -> ('a, char -> 'a) fmt *)
let fmt3 () = fmt31 () ^^ f_int ()
(* val fmt3 : unit -> ('a, char -> int -> 'a) fmt *)

let tp3 = sprintf (fmt3 ()) 'x' 3
(* val tp3 : string = "The value of x is 3" *)

(* What we print, we can parse back *)
let ts3 = sscanf tp3 (fmt3 ()) (fun x n -> (x,n))
(* val ts3 : (char * int) * string = (('x', 3), "") *)



(* ------------------------------------------------------------------------ *)
(* Example 2:

   Simply-typed lambda-calculus with constants and higher-order abstract
   syntax. This is essentially the example of

      Guarded Recursive Datatype Constructors
      Hongwei Xi, Chiyan Chen and Gang Chen
      POPL 2003
*)

(*
   We would love to be able to define the type of expressions 'a exp
   as follows:

   type 'a exp =
      | Int of (int, 'a) eq * int                (* Integer literal *)
      | Lft of ('a,'a) eq * 'a                   (* Lifting any constant *)
      | Inc of (int->int, 'a) eq                 (* Increment *)
      | Lam of exists 'u 'v.
          ('u -> 'v,'a) eq * ('u exp -> 'v exp)  (* Lambda, HOAS *)
      | App of exists 'u 'v.                     (* Application *)
          ('v,'a) eq * ('u -> 'v) exp * 'u exp

   Alas, OCaml syntax -- the lack of existential quantification --
   prevents this. We settle with encoding existentials as universals, using
   double-negation.
*)

type 'a exp =
   | Int of (int,'a) eq * int
   | Lft of ('a,'a) eq * 'a
   | Inc of (int->int,'a) eq
   | Lam of < m_lam : 'w. ('a,'w) lam_k -> 'w >
   | App of < m_app : 'w. ('a,'w) app_k -> 'w >
      (* The standard encoding of existentials *)
and ('a,'w) lam_k =
    {lam_k : 'u 'v. (('u -> 'v),'a) eq * ('u exp -> 'v exp) -> 'w}
and ('a,'w) app_k =
    {app_k : 'u 'v. ('v, 'a) eq * ('u -> 'v) exp * 'u exp -> 'w}


(* smart constructors, as syntactic sugar *)
(* Method signatures are, alas, required. *)
let e_int x = Int (refl (), x)
(* val e_int : int -> int exp *)

let e_lft x = Lft (refl (), x)
(* val e_lft : 'a -> 'a exp *)

let e_inc = Inc (refl ())
(* val e_inc : (int -> int) exp *)

let e_lam f = Lam (object method m_lam : 'w. ('a,'w) lam_k -> 'w
   = fun k -> k.lam_k (refl (),f) end)
(* val e_lam : ('a exp -> 'b exp) -> ('a -> 'b) exp *)

let e_app f x = App (object method m_app : 'w. ('a,'w) app_k -> 'w
   = fun k -> k.app_k (refl (),f,x) end)
(* val e_app : ('a -> 'b) exp -> 'a exp -> 'b exp *)


(* Test expressions *)
(* Note their inferred types given in comments *)
let test1 = e_app e_inc (e_int 1)
(* val test1 : int exp *)

let test2 = e_lam (fun x -> e_app e_inc (e_app e_inc x))
(* val test2 : (int -> int) exp *)

let test3 = e_lam (fun f -> e_lam (fun x -> e_app f x))
(* val test3 : (('_a -> '_b) -> '_a -> '_b) exp *)

let test4 = e_app (e_app test3 test2) (e_int 3)
(* val test4 : int exp *)

(* The interpreter. We need polymorphic recursion -- as is
   typical with GADTs*)

let rec eval : 'a. 'a exp -> 'a =
 function
   | Int (eq, x) -> cast eq x
   | Lft (eq, x) -> x
   | Inc eq -> cast eq succ
   | Lam x ->
      x#m_lam {lam_k =
          fun (eq,f) -> cast eq (fun x -> eval (f (e_lft x)))}
   | App x ->
       x#m_app {app_k =
          fun (eq,f,x) -> cast eq ((eval f) (eval x))}


let test1_ev = eval test1 (* int = 2 *)
let test2_ev = eval test2 (* int -> int = <fun> *)
let test3_ev = eval test3 (* (int -> int) -> int -> int = <fun> *)
let test4_ev = eval test4 (* int = 5 *)
