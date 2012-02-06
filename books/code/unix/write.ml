open BatPervasives
open Printf


external unsafe_single_write :
  Unix.file_descr -> string -> int -> int -> int = "caml_single_write"

let single_write fd buf ofs len =
  if  ofs < 0 || ofs + len > String.length buf
  then invalid_arg "single_write"
  else unsafe_single_write fd buf ofs len 


















