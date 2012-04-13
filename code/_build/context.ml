open Batteries

(**
   Grammar
   L := w C w
   w := (A|B)*
*)
type token = A |B |C
    
let rec parser1 = parser
  | [< 'A ; l = parser1 >] -> (parser [< 'A>] -> "a") :: l 
  | [< 'B ; l = parser1 >] -> (parser [< 'B>] -> "b") :: l 
  | [<>] -> []  (* always succeed *)
let parser2 lst str =
  List.fold_left (fun s p -> p str ^ s) ""  lst 
let parser_L = parser
  | [< ls = parser1 ; 'C; r = parser2 ls >] ->
    r
let _ =
  [A;B;A;B;C;A;B;A;B]
  |>  Stream.of_list
  |>  parser_L
  |> print_endline

(* let a = *)
(*   let open A in *)
(*   let a = 3 in *)
(*   let b = 3 in *)
(*   let d = 3 in *)
(*   d *)

let a=
  let open Pervasives in 
  let c = 5 in
  c



