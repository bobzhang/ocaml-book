open BatPervasives
open Printf

open Camlp4.PreCast
module MGram = MakeGram(Lexer)

let record = MGram.Entry.mk "record"
let _ = begin
  MGram.Entry.clear record;
  EXTEND MGram GLOBAL:record ;
  record:
    [ [ "{"; xys = sequence; OPT ","; "}" ->Some (Array.of_list xys)
      | "%" -> None ]
	
    ];
  sequence:
    [ [ `INT(x,_); `INT(y,_); xs= sequence'  ->
       if y<> 0
       then x::xs
       else xs ] ];
  sequence':
    [ [   -> [] | ","  ->  [] | ","; el = sequence -> el] ];
  END
end 

let record_p = MGram.parse_string record (Loc.mk "<>")




















