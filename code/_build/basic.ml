





















let _ =
  print_endline "basic" 
  
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
