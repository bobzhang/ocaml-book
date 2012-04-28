open Format

(* SomeModule.ml *)
let say_hello () = print_endline "Hello, world!"
let () = Hashtbl.replace Reg.registry "say_hello" say_hello


















