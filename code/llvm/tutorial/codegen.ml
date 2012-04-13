(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: codegen.ml,v 0.0 2012/02/21 04:55:19 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf
open Util
open Llvm 
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

exception Error of string
module Hashtbl = struct
  include Hashtbl
  let add tbl k v = begin
    prerr_endlinef "add %s to the hashtbl" k;
    add tbl k v
  end
  let clear tbl = begin
    prerr_endlinef "clear hashtbl";
    clear tbl  
  end 
end
  
let cxt : llcontext = global_context ()

(** contains all of the functions and global variables in a chunk of
    code
*)
let the_module : llmodule= create_module cxt "jit"

let builder : llbuilder = builder cxt

(** for this example, the only thing that can be referenced is
    function parameters*)
let named_values : (string,llvalue) Hashtbl.t = Hashtbl.create 10

let double_type : lltype = double_type cxt



(** creates the JIT *)  
let the_execution_engine : ExecutionEngine.t =
  ExecutionEngine.create the_module
let the_pfm = PassManager.create_function the_module
(** Set up the optimizer pipeline Start with registering info about
    how the target lays out data structure *)
let _ = begin 
  TargetData.add (ExecutionEngine.target_data the_execution_engine) the_pfm;
  add_instruction_combination the_pfm;
  add_reassociation the_pfm;
  add_gvn the_pfm;
  add_cfg_simplification the_pfm;
  PassManager.initialize the_pfm |> ignore;
end 

let ($+$) l r = build_fadd l r "addtmp" builder
let ($-$) l r = build_fsub l r "subtmp" builder
let ($*$) l r = build_fmul l r "multmp" builder
let ($<$) l r = build_fcmp Fcmp.Ult l r "cmptmp" builder
  
let rec code_of_expr e = Tast.(
  match e with
    | Number n ->
      (** ConstantFP *)
      const_float double_type n
    | Variable name ->
      (try Hashtbl.find named_values name
       with Not_found -> raise (Error "unknown variable name"))
    | Binary (op,lhs,rhs) ->
      let lval = code_of_expr lhs
      and rval = code_of_expr rhs in (
	match op with 
	  | '+' ->  lval $+$ rval
	  | '-' ->  lval $-$ rval 
	  | '*' ->  lval $*$ rval 
	  | '<' ->
	    let i = lval $<$ rval in 
	    (** uitofp instruction *)
	    build_uitofp i double_type "booltmp" builder
	  | _ -> raise ^$ Error "invalid binary operator"
      )
    | Call(f,args) ->
      let callee =
	match lookup_function f the_module with
	  | Some f' -> f'
	  | None -> begin
	    prerr_endlinef "unknown function %s" f ;
	    raise ^$ Error "unknon function";
	  end in
      let params = params  callee in (
	assert (Array.length params = Array.length args) ;
	let args  = Array.map code_of_expr args  in
	build_call callee args "calltmp" builder (** *)
      )
)

let rec code_of_proto e = Tast.(
  match e with
    | Prototype (name,args) ->
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f =
	match lookup_function name the_module with
	  | None ->
	    (** the function may be defined outside the current
		module *)
	    declare_function name ft the_module
	  | Some f ->
	    (if block_begin f <> At_end f then
		raise ^$ Error "redefinition of function" 
	     else if element_type (type_of f) <> ft then
	       raise ^$ Error "redefinition of function with different #args" 
	     else f
	  ) in
      Array.iteri (fun i a ->
	let n =args.(i) in
        set_value_name n a ;
	Hashtbl.add named_values n a 
      ) (params f);
      f
)

let code_of_func e = Tast.(
  match e with
    | Function (proto,body) ->
      Hashtbl.clear named_values;
      let the_function = code_of_proto proto in
      let bb = append_block cxt "entry" the_function in
      position_at_end bb builder;
      try
	let ret_val = code_of_expr body in
	let _ = build_ret ret_val builder in
	Llvm_analysis.assert_valid_function the_function;

	(** PassManager optimize pass *)
	prerr_endlinef "before optimization ---> ";
	dump_value the_function;
	let _ = PassManager.run_function the_function the_pfm in begin 
	prerr_endlinef "after optimization ---> ";
	the_function;
	end 
      with e -> 
	delete_function the_function;
	raise e
)

let code_of_prog e= Tast.(
  match e with
    | Func x ->
      prerr_endlinef "%s\n --->\n" (string_of_func x);
      code_of_func x 
    | Proto x ->
      prerr_endlinef "%s\n --->\n" (string_of_proto x);
      code_of_proto x
    | Expr x ->
      prerr_endlinef "%s\n --->\n" (string_of_expr x);
      code_of_expr x 
)  

let code_of_progs  =
  List.map
    (fun p ->
      try
	let v = code_of_prog p in
	(dump_value v; Some v)
      with exn -> begin
	prerr_endlinef " error translation %s\n msg:%s"
	  (Tast.string_of_prog p) (Printexc.to_string exn);
	None
      end )

(* let jit_of_progs = *)
(*   List.iter *)
(*     Tast.(fun p -> *)
(*       match p with *)
(* 	| Func _ | Proto _ -> *)
(* 	  code_of_prog  p |> dump_value; () *)
(* 	| Expr x -> *)
(* 	  let expr = code_of_prog p in *)
(* 	  expr|> dump_value; *)
(* 	  () *)
(* 	  (\* let result = ExecutionEngine.run_function expr [||] *\) *)
(* 	  (\*   the_execution_engine in *\) *)
(* 	  (\* prerr_endlinef "evaluated to\n --->\n %f" *\) *)
(* 	  (\* (GenericValue.as_float double_type result); *\) *)
(*     ) *)

