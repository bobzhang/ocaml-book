open BatPervasives
open Printf
open Util

exception Exited
type 'a result = Value of 'a | Exception of exn

let eval f x =
  try Value (f x )
  with z -> Exception z

(** evaluate a computation in another thread
    and wait for the result
*)
let coexec (f : 'a -> 'b )  (x : 'a) : unit -> 'b = Thread.(
  let result = ref (Exception Exited) in
  let p = Thread.create (fun x -> result := eval f x ) x in
  function () -> match (join p; !result) with
    | Value v -> v
    | Exception exn -> raise exn 
)

















