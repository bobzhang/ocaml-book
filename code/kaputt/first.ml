open Printf;

open Kaputt.Abbreviations ;

(* let _ = *)
(*   let open Test in *)
(*   add_enum_test *)
(*     ~title:"int" *)
(*     (Enum.int 1 5) *)
(*     (fun _ -> ()) *)
(*     (Spec.always ==> Spec.never) *)


Test.add_simple_test
  ~title:"miscellaneous"
    (fun () -> begin 
      Assert.is_true True;
      Assert.is_false False;
      Assert.is_some (Some 3);
      Assert.is_none None;
      Assert.raises (fun () -> failwith "msg");
      Assert.no_raise ignore;
      Assert.make_raises
        (fun [Not_found -> True | _ -> False])
        Printexc.to_string
        (fun () -> raise Not_found)
     end);





value seed = 3;
  
Test.add_random_test
    ~title:"int * float"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.zip2
      (Gen.make_int 1 1000)
      (Gen.transform (fun x -> mod_float x 1000.) Gen.float))
    (fun (x, y) -> (float x) +. y)
    [Spec.always ==> Spec.never]
;


Test.add_random_test
    ~title:"string option"
    ~nb_runs:3
    ~random_src:(Gen.make_random_seed seed)
    (Gen.option Gen.bool (Gen.word (Gen.make_int 1 9)))
    (fun _ -> 0)
    [Spec.always ==> Spec.never]
;

Test.add_enum_test
    ~title:"int"
    (Enum.int 1 5)
    (fun _ -> ())
    [Spec.always ==> Spec.never]
;

Test.add_enum_test
    ~title:"[1..10 | even]"
    (Enum.filter (fun x -> (x mod 2) = 0) (Enum.int 1 10))
    (fun _ -> ())
    [Spec.always ==> Spec.never]
;
  

Test.add_enum_test
    ~title:"[1..10] [13..15]"
    (Enum.sequence [Enum.int 1 10; Enum.int 13 15])
    (fun _ -> ())
    [Spec.always ==> Spec.never]
;    

check Gen.int succ [Spec.is_odd_int ==> Spec.is_even_int ]
;    


Test.run_tests [] ; 
