(* pa_refutable : Camlp4 (3.10) Syntax extension

Input :
  (\ bar \2 \1 )
Output :
  (fun __hole_1 __hole_2 -> bar __hole_2 __hole_1)

- \0 isn't legal, we start at \1
- the number of parameters defined is the maximum
   of all \n inside the (\ ... ) :
   (\ \3 ) will bind three parameters and ignore the first two
- nested (\ ..) content is not processed during the computation
  of the "maximum parameter number" : in the (\ 1 (\ \2 )) case,
  the outer (\ .. ) will only declare one parameter


Compilation :
ocamlfind ocamlc -pp camlp4rf -package camlp4 -c pa_holes.ml 

Use :
camlp4o pa_holes.cmo test.ml
ocamlc -pp 'camlp4o pa_holes.cmo' test.ml

*)

(* Copyright (C) 2007-

      Author: Bluestorm
      email: bluestorm dot dylc on-the-server gmail dot com

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

open Camlp4;

module Id : Sig.Id = struct
  value name = "Holes as lambda";
  value version = "0.2.1";
end;

value tail str = String.sub str 1 (String.length str - 1);
value num_value str = int_of_string (tail str);
value is_num str = try ignore (num_value str); str.[0] = '\\' with [ _ -> False ];    

module Add_holes (Syntax : Sig.Camlp4Syntax) = struct
  open Sig;
  include Syntax;

  (* /!\ side effect :
     the processor combines mapping over the expression to
     translates the \n terms into regular idents (using hole_prefix),
     and collecting the maximum \n into the max_hole reference.
     The reference can be acessed with the #hole_count method.
     A fresh reference is created at each process_holes call.
  *)
  value process_holes hole_prefix =
    let max_hole = ref 0 in
  object
    inherit Ast.map as super;
    method hole_count = !max_hole;
    method ident = fun
      [ <:ident@_loc< $lid:str$ >> when is_num str && num_value str > 0 ->
        let hole = num_value str in
        let () = max_hole.val := max hole !max_hole in
         <:ident< $lid: hole_prefix ^ string_of_int hole$ >>
      | other -> super#ident other ];
  end;

  value hole expr _loc =
	let hole_prefix = "__hole_" in (* TODO : use FV instead *)
	let processor = process_holes hole_prefix in
	let body = processor#expr expr in
	let count = processor#hole_count in
	let rec add_hole expr = fun
    [ 0 -> expr
    | n -> add_hole <:expr< fun $lid:hole_prefix ^ string_of_int n$ -> $expr$ >> (n -1) ]
	in
    add_hole body count;
    
  EXTEND Gram
    expr: LEVEL "simple" [[
      "("; "\\"; e = SELF; ")" -> hole e _loc
    ]];
    ident: [[
      "\\"; n = a_INT -> <:ident< $lid:"\\" ^ n$ >>
    ]];
    END;
end;


(* The filter trick is used to disallow \n idents outside (\ ... ) :
   after the (\ .. ) have been computed and the \n inside removed,
   a filter process the whole AST and raise an error on any \n left *)
module Remove_holes (Filters : Camlp4.Sig.AstFilters) = struct
  open Filters;
  
  value no_hole = object
    inherit Ast.map as super;
    method ident = fun
      [ <:ident@loc< $lid:str$ >> when is_num str -> Ast.Loc.raise loc (Failure 
           (Printf.sprintf "Holes ( %s ) are not allowed outside (\\ ... )" str))
      | other -> super#ident other ];
  end;

  register_str_item_filter no_hole#str_item;
end;

let module M = Register.OCamlSyntaxExtension Id Add_holes in ();
let module M = Register.AstFilter Id Remove_holes in ();
