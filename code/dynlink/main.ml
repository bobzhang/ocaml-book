open Format
(* Main program *)
let filename = "some_module.cmo"
let funcname = "say_hello"
let () =
  Dynlink.init ();
  (try Dynlink.loadfile filename
   with Dynlink.Error e -> failwith (Dynlink.error_message e));
  (Hashtbl.find Reg.registry funcname) ()


















