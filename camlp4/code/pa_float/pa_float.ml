(* Author: bobzhang1988@vpl472.wlan.library.upenn.edu        *)
(* Version: $Id: pa_float.ml,v 0.0 2012/02/15 17:43:44 bobzhang1988 Exp $ *)

open Printf;
open Camlp4.PreCast;


(* value transform_obj = object *)
(*   inherit Ast.map as super; *)
(*   method expr e = *)
(*     super#expr (match e with *)
(*     [ <:expr@_loc< (+) >> -> <:expr< (+.) >> *)
(*     | <:expr@_loc< ( * ) >> -> <:expr< ( *. ) >> *)
(*     | <:expr@_loc< ( - ) >> -> <:expr< ( -. ) >> *)
(*     | <:expr@_loc< ( / ) >> -> <:expr< (/.) >> *)
(*     (\** Notice that `int, int makes difference only when it appear in expr context *)
(* 	Since the Ast is defined as Ast.ExInt(loc,string), you can not pattern match get *)
(* 	an int *)
(*     *\) *)
(*     | <:expr@_loc< $int:i$ >> *)
(*       -> <:expr< $`flo:(float  (int_of_string i))$ >> *)
(*     |  e ->  e  *)
(*       ]); *)
(* end ; *)
value transform_obj = object
  inherit Ast.map as super;
  method expr e =
    super#expr (
      match e with
	  [ <:expr@_loc< $lid:op$ >>
	      when List.mem op ["+";"-"; "*";"/" ]
		-> <:expr< $lid:op^"."$>>
	  | <:expr@_loc<$int:i$ >> ->
	    <:expr< $flo:i$ >>
	  | e -> e ]
    );
end ;


value transform = transform_obj#expr ;

(* let expression = Gram.Entry.mk "expression" *)

open Syntax;


begin
  EXTEND Gram GLOBAL:expr;
  (** a simple delimited context, and put in a high priority
      just like ( )
  *)
  expr:
    [  "simple"
	["[^"; e = SELF  ;"^]" ->
	transform e]
    ];
  END;
end ;
  


















