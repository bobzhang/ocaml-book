

external inspect : 'a ->'a = "inspect_block"
external explore_string : string->unit="explore_string"
let a = ref 111333
let f b =
  !a + 3
let _ =
  (* let _ = inspect 3 in *)
  (* let _ = inspect "ghso" in *)
  (* let _ = inspect 'a' in *)
  (* let _ = inspect 32l in (\* boxed *\) *)
  (* let _ = inspect f in  *)
  (* inspect 32.0 (\* float is boxed*\) *)
  begin
    (* inspect f; *)
    (* inspect [1;2;3;4]; *)
    (* inspect "ghsogho\000ghoshg"; *)
    (* explore_string "ghsogho\000ghoshg"; *)
    (* explore_string "abcdefg"; *)
    (* explore_string "abcdefgh"; *)
    (* explore_string "abcdefgh\000"; *)
    (* inspect 1.0; *)
    (* inspect [|1.0;2.0|]; *)
    (* inspect [|1;2|];   *)
    (* inspect [3.13;23.0]; (\* boxed *\) *)
    (* inspect (fun x y z -> x + y + z); *)
    (* let g x = let y = 2 in fun z -> x + y + z in *)
    (* let a1 = g 1 in  *)
    (* inspect a1; *)
    (* let p = Stack.create () in  (\** Stack.t is abstract but not Abstract_tag *\) *)
    (* Stack.push 3 p ; *)
    (* Stack.push 2 p ; *)
    (* inspect p *)
    (* let w = Weak.create 10 in *)
    (* Weak.set w 0 (Some 3); *)
    (* inspect w *)
    (* inspect stdout *)
  end

external mirror : string->string="swap_string"
let _ =
  print_string (mirror "ghsogho")
