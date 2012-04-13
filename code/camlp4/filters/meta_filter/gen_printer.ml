(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: gen_printer.ml,v 0.0 2012/03/09 22:20:48 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf;
open Camlp4.PreCast;
open Util;


value filter = object
  inherit Ast.map as super;
  method! str_item x = 
    (match x with
    [ <:str_item@_loc< __gen__pp__( $l$ ) >> -> begin

      let ls = list_of_expr_list l  in
      let strs =
	List.map (fun
	  [ <:expr@_loc<$lid:s$ >> ->
	    <:str_item<
	    
	    value $lid: "p_" ^ s ^ "_r"$ =
		Format.(fprintf err_formatter "@[%a@]@."
 			  opr# $lid:s$ );
   		       (* opr# $lid:s$ Format.std_formatter; *)
	    value $lid: "p_" ^ s ^ "_o"$ =
   		Format.(fprintf err_formatter "@[%a@]@."
			  opo# $lid:s$ );
			  (* opo# $lid:s$ Format.std_formatter; *)
	    
	    value $lid:"s_" ^ s ^"_r"$ e =
		let buf = Buffer.create 50 in
		let fmt = Format.formatter_of_buffer buf in begin
		  Format.(fprintf fmt "@[%a@]@."
			    opr# $lid:s$ e 
			 );
		  (* opr# $lid:s$ fmt e ; *)
		  (* Format.pp_print_flush fmt (); *)
		  Buffer.contents buf ;
		end ;
		
	    value $lid:"s_" ^ s ^"_o"$ e =
		let buf = Buffer.create 50 in
		let fmt = Format.formatter_of_buffer buf in begin
		  Format.(fprintf fmt "@[%a@]@."
			 opo# $lid:s$ e);
		  (* opo# $lid:s$ fmt e ; *)
		  (* Format.pp_print_flush fmt (); *)
		  Buffer.contents buf 
		end ;		
					  >>
	  | _ -> begin
	      prerr_endline "lid expected in __gen__pp__ ";
	      assert False;
	  end ]) ls  in 
      (* prerr_endlinef "%s " names; *)
      (** FIXME tidy the interface later
	  probably, we could generate from .cmi file
       *)
      <:str_item@_loc<
      module RPrinters  (* : sig
	value printer : unit -> Ast.Printer ;
      end*)  = Camlp4.Printers.OCamlr.Make(Camlp4.PreCast.Syntax);
      module OPrinters
	  = Camlp4.Printers.OCaml.Make(Camlp4.PreCast.Syntax);
      value opo  = (new OPrinters.printer());
      value opr = (new RPrinters.printer());
      $list: strs $;
      (* $super#str_item x $ *)
      >>
      end
    | <:str_item@_loc< __gen__parser__ ($l$ )>> -> begin
	let ls = list_of_expr_list l in
	let strs =
	  List.map (fun
		   [ <:expr@_loc< $lid:s$ >> ->
		     <:str_item<
		     value $lid:"parse_"^s$ s =
			try 		  
			  Camlp4.PreCast.Syntax.Gram.parse_string
			    Camlp4.PreCast.Syntax.$lid:s$ (Camlp4.PreCast.Loc.mk "<string>") s 
			with
			  [ Camlp4.PreCast.Loc.Exc_located t exn -> begin 
			    prerr_endline (Camlp4.PreCast.Loc.to_string t);
			    raise exn ;
			  end 
			  ]
		     >>
	           | _ -> begin
		       prerr_endline "lid expected in __gen__parser__ ";
		       assert False;
		   end 
		   ]) ls in
	<:str_item@_loc<
 	$list:strs$	    
		    >>;
		    
    end
	  
    | _ ->     super#str_item x
   ]); 

end;


AstFilters.register_str_item_filter (filter#str_item);


















