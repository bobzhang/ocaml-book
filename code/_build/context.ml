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
(**
assert_failure, assert_equal, @?, assert_raises, skip_if, todo, cmp_float
bracket 
*)
let test_result = OUnit.(
  run_test_tt ("test-suite" >::: 
                  ["test2" >:: (fun _ -> ());
                   "test1" >:: (fun _ -> "true" @? true)
                  ]
  ))
;;

(**Remark
*)
