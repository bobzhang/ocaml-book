
open Camlp4.PreCast
  
let lexer = Lexer.mk () Loc.ghost

let lexer_of_string str =
  let streams = str
    |> Stream.of_string
    |> lexer  in
  try Stream.iter (fun (token,loc) ->
    let fmt = Format.std_formatter in begin
      if token <> EOI then begin 
	Token.print fmt token;
	Loc.print fmt loc;
	Format.print_newline ();
      end
      else raise End_of_file
    end ) streams
  with End_of_file -> ()

let _ =  lexer_of_string "#a<gsg,,,!>!?";;
(*
SYMBOL "#"File "ghost-location", line 1, characters 0-1
LIDENT "a"File "ghost-location", line 1, characters 1-2
SYMBOL "<"File "ghost-location", line 1, characters 2-3
LIDENT "gsg"File "ghost-location", line 1, characters 3-6
SYMBOL ","File "ghost-location", line 1, characters 6-7
SYMBOL ","File "ghost-location", line 1, characters 7-8
SYMBOL ","File "ghost-location", line 1, characters 8-9
SYMBOL "!>!?"File "ghost-location", line 1, characters 9-13
*)
