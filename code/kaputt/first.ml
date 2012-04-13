(* -*- Mode:caml; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: first.ml,v 0.0 2012/02/27 01:01:10 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf;;

open Kaputt.Abbreviations ;;

let _ =
  let open Test in
  add_enum_test
    ~title:"int"
    (Enum.int 1 5)
    (fun _ -> ())
    (Spec.always ==> Spec.never)




















