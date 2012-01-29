

let a = Array.make 10 0
let f () = a.(11)

(** only one match case in the toplevel *)  
let f2 x = match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3

(** 3 match cases in the toplevel *)    
let f3 = function
  | 0 -> 1
  | 1 -> 2
  | 3 -> 4 
let g = f
let h = g
let main = h ()

(**
   camlp4of -I _build pa_exception_wrapper.cmo test_pa_exception_wrapper.ml -printer o

let a = Array.make 10 0
  
let f () =
  try a.(11)
  with
  | exc ->
      (Format.eprintf
         "Exception tracer at File "/Users/bobzhang1988/Writing/ocaml-book/camlp4/examples/test_pa_exception_wrapper.ml", line 4, characters 11-17 (%s)@."
         (Printexc.to_string exc);
       raise exc)
  
(** only one match case in the toplevel *)
let f2 x =
  try match x with | 0 -> 1 | 1 -> 2 | 2 -> 3
  with
  | exc ->
      (Format.eprintf
         "Exception tracer at File "/Users/bobzhang1988/Writing/ocaml-book/camlp4/examples/test_pa_exception_wrapper.ml", line 7, characters 11-56 (end at line 10, character 10) (%s)@."
         (Printexc.to_string exc);
       raise exc)
  
(** 3 match cases in the toplevel *)
let f3 =
  function
  | 0 ->
      (try 1
       with
       | exc ->
           (Format.eprintf
              "Exception tracer at File "/Users/bobzhang1988/Writing/ocaml-book/camlp4/examples/test_pa_exception_wrapper.ml", line 14, characters 9-10 (%s)@."
              (Printexc.to_string exc);
            raise exc))
  | 1 ->
      (try 2
       with
       | exc ->
           (Format.eprintf
              "Exception tracer at File "/Users/bobzhang1988/Writing/ocaml-book/camlp4/examples/test_pa_exception_wrapper.ml", line 15, characters 9-10 (%s)@."
              (Printexc.to_string exc);
            raise exc))
  | 3 ->
      (try 4
       with
       | exc ->
           (Format.eprintf
              "Exception tracer at File "/Users/bobzhang1988/Writing/ocaml-book/camlp4/examples/test_pa_exception_wrapper.ml", line 16, characters 9-10 (%s)@."
              (Printexc.to_string exc);
            raise exc))
  
let g = f
  
let h = g
  
let main = h ()
  

  

   
*)  
