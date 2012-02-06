open BatPervasives
open Printf


open Util


let _  = begin
  for i = 0 to 100 do
    print_protocol_entry stdout (Unix.getprotobynumber i);
    print_newline ()  ;
  done ;
end 


















