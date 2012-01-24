
open Printf
  
let u8l = Ulexing.utf8_lexeme
let stk = Stack.create ()
let indent = ref 0 
let prerr_string str = begin 
  prerr_string (String.make (2 * !indent) ' ');
  prerr_string str 
end 
exception Not_match of string * (int * int)
    
let rec step_env = lexer
  |"\\begin{" [^ '}']+ "}" ->
    let l = u8l lexbuf in
    let len = String.length l in
    let env = String.sub l 7 (len - 8) in begin
      incr indent ;
      prerr_string (sprintf "%s\n" env);
      Stack.push env stk;
      step_env lexbuf;
    end 
  |"\\end{" [^ '}']+ "}" ->
    let l = u8l lexbuf in 
    let len = String.length l in
    let env = String.sub l 5 (len - 6) in
    let match_env = Stack.pop stk in
    if env = match_env then begin
      prerr_string (sprintf "%s\n" env);
      decr indent;
      step_env lexbuf;
    end 
    else begin
      let s,f = Ulexing.loc lexbuf in 
      prerr_string (sprintf "unexpected env %s, expected %s at (%d,%d)\n"
		      env match_env s f);
      raise (Not_match ("",(s,f)))
    end 
  | _ ->
    step_env lexbuf
  | eof ->
    if not (Stack.is_empty stk) then begin
      let s,f = Ulexing.loc lexbuf in 
      prerr_string (sprintf "unexpected end of input\n in the stack: \n");
      Stack.iter (fun s -> prerr_string s ; prerr_newline ()) stk;
      raise (Not_match ("",(s,f)))
    end 
    else begin
    end 

    

let check filename =
  prerr_string (sprintf "check %s\n" filename);
  let chan = open_in filename in
  let lexbuf = Ulexing.from_utf8_channel chan in
  begin
    try 
      step_env lexbuf;
    with Not_match(_,pos)-> raise (Not_match (filename,pos))
    close_in chan
  end

(* let _ = begin *)
(*   check "../camlp4/extensible_parser.tex" *)
(* end  *)

  
	  
