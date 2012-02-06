


(** The backtrace lists the program locations where the most-recently
    raised exception was raised and where it was propagated through
    function calls. *)
let f () = raise Not_found

let g () = let _ = f () in  3

let _ = 
  try g ();
      assert false
  with Not_found ->
    prerr_endline "fuck";
    Printexc.print_backtrace stdout;
    

(**
   OCAMLRUNPARAM=b ./trace.d.byte
   fuck
   Raised at file "trace.ml", line 2, characters 17-26
   Called from file "trace.ml", line 4, characters 19-23
   Called from file "trace.ml", line 7, characters 6-10

*)
