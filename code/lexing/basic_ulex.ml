let regexp op_ar = ['+' '-' '*' '/']
let regexp op_bool = ['!' '&' '|']
let regexp rel = ['=' '<' '>']

let (|>) x f = f x 
(** get string output, not int array *)  
let lexeme = Ulexing.utf8_lexeme
  
let rec basic = lexer
  | [' '] -> basic lexbuf 
  | op_ar | op_bool ->
    let ar = lexeme lexbuf in 
    `Lsymbol ar
  | "<=" | ">="| "<>" | rel ->
    `Lsymbol (lexeme lexbuf)
  |("REM" | "LET" | "PRINT"
       | "INPUT" | "IF"| "THEN") ->
    `Lsymbol (lexeme lexbuf)
  | '-'?['0'-'9']+ ->
    `Lint (int_of_string (lexeme lexbuf))
  | ['A'-'z']+ ->
    `Lident (lexeme lexbuf)
  | '"' [^ '"'] '"' ->
    `Lstring (let s = lexeme lexbuf in
              String.sub s 1 (String.length s - 2))
  | eof -> raise End_of_file    
  | _ ->
    (print_endline (lexeme lexbuf ^ "unrecognized");
    basic lexbuf)

let token_of_string str =
  str
  |> Stream.of_string
  |> Ulexing.from_utf8_stream
  |> basic
let tokens_of_string str =
  let output = ref [] in
  let lexbuf = str |> Stream.of_string |> Ulexing.from_utf8_stream in
  (try 
    while true do
    let token = basic lexbuf in 
    output:= token :: !output
    done
  with End_of_file -> ());
  List.rev (!output)   





let _ = tokens_of_string
  "a + b >= 3 > 3 < xx"






