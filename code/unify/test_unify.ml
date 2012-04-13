(* Author: bobzhang1988@vpl729.wireless-pennnet.upenn.edu        *)
(* Version: $Id: test_unify.ml,v 0.0 2012/02/16 21:31:51 bobzhang1988 Exp $ *)
open Printf
open BatPervasives
let res  =
  try
    BatFile.lines_of "unify_test.data"
  |> BatEnum.map (fun line -> begin
    print_string line ;
    print_newline ();
    pair_of_string line end)
  |> BatList.of_enum
  with Loc.Exc_located (t,exn) -> begin
    prerr_endline (Loc.to_string t);
    prerr_endline (Printexc.to_string exn);
    raise exn;
  end 


















