(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: simple.ml,v 0.0 2012/03/03 13:08:09 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf


let pp_cell fmt cell = Format.fprintf fmt "%s" cell

let rec pp_list ?(sep=" ") pp_element fmt = function
  | [h] -> Format.fprintf fmt "%a" pp_element h
  | h::t ->
      (* @, *)
      Format.fprintf fmt "%a%s@,%a"
	pp_element h sep (pp_list ~sep pp_element) t 
  | [] -> ()

let fmt = Format.std_formatter

let _ =  begin
  let open Format in
  let data = ["aa";"bb";"cc"] in 
  fprintf fmt "@[<v>%a@]@." (pp_list ~sep:"," pp_cell) data;
  fprintf fmt "@[<h>%a@]@." (pp_list ~sep:"," pp_cell) data;
  fprintf fmt "@[<v 1>@,%a@]@." (pp_list ~sep:"," pp_cell) data;
end 

(** soft break @, -- once as newline, once as space 
    @[<v>@]
    @[<h>@]
    @[<h 1>@]
    @[<v 1>@] -- identation
    @[@]
    @.
 *)    

(* let pp_header widths fmt header = *)
(*   let first_row = Array.map *)
(*       (fun x -> String.make (x+1) ' ') widths in *)
(*   Array.iteri (fun j cell -> begin *)
(*     Format.pp_set_tab fmt (); *)
(*     for z = 0 to (String.length header.(j)) - 1 do *)
(*       cell.[z] <- header.(j).[z] *)
(*     done ; *)
(*   end) first_row  *)

let pretty_decl (ppf:Format.formatter)
    name (l: string list) =
  let open Format in 
  fprintf ppf "@[<hv 4>%s {" name;
  List.iter (fun instr -> fprintf ppf "@ %s;" instr) l;
  fprintf ppf "@;<1 -4>}@]"


type lambda =
  | Lambda of string * lambda
  | Var of string
  | Apply of lambda * lambda
	
open Format

let ident = print_string
let kwd = print_string     
let fmt = Format.std_formatter
    
let rec print_exp0 fmt  = function
  | Var s -> ident s
  | lam ->
      printf "@[<1>(%a)@]" print_lambda  lam
and print_app  fmt  = function
  | e -> printf "@[<2>%a@]" print_other_applications e
and print_other_applications fmt (f:lambda) =
  match f with
  | Apply (f,arg) ->
      printf "%a@ %a" print_app f print_exp0 arg
  | f -> print_exp0 fmt f 
and print_lambda  fmt = function
  | Lambda(s,lam) ->
      printf "@[<1>\\%s.@ %a@]" s print_lambda lam
  | e -> print_app fmt e 







