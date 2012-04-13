(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: test_ast_parser.ml,v 0.0 2012/02/21 02:48:25 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)


(* #directory "_build";; *)

(* #load "ast_parser.cma";; *)

open Llvm;;
open Printf;;
open Util;;
open Tast;;
open Ast_parser;;
open Codegen;;
open Llvm_executionengine;;
open Llvm_target;;
open Llvm_scalar_opts;;

(* open Codegen;; *)

#install_printer print_expr;;
#install_printer print_progs;;

(* let a = BatFile.lines_of "test_file.py" *)
(*   |> BatEnum.fold (fun x y -> x ^"\n"^ y) "" *)
(*   |> progs_eoi_p *)
(*   |> print_progs ;; *)

let from_file file =
  file
  |>  BatFile.lines_of
  |>  BatEnum.fold (fun x y -> x ^ "\n" ^ y) ""
  |>  progs_eoi_p 


let _ =
  "test.ml"
  |> from_file
  |> code_of_progs

let code_from_string str =
  str
  |> progs_eoi_p
  |> code_of_progs 

let _ = begin
  "def xx() sin(1.) * sin(1.) + cos(1.) * cos(1.); "
  |> code_from_string
  |> (fun ( [Some f ]  ) -> ExecutionEngine.run_function f [| |] the_execution_engine )
  |> (fun r -> prerr_endlinef "result --> %f" (GenericValue.as_float double_type r))
      
end 
(** need declaration extern sin cos  first *)      
(*
let _ = begin
  "def xx5() ff(1.);"
  |> code_from_string
  |> (fun ( [Some f] ) ->  ExecutionEngine.run_function  f  [| |] the_execution_engine)
  |> (fun r -> prerr_endlinef "result --> %f "
        (GenericValue.as_float double_type r))
  ;
end 
*)
