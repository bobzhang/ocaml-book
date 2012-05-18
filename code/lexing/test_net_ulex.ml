open Netulex
let digits = lexer
   | ['0'-'9']+ -> `Number(int_of_string(Ulexing.utf8_lexeme lexbuf))
   | 8364       -> `Euro_sign   (* Code point #8364 in Unicode *)

         
let sample = "3242543\226\130\172";;
let ulb = ULB.from_string `Enc_utf8 sample;;
let lexbuf = Ulexing.from_ulb_lexbuf ulb;;
let first_token,second_token  = (digits lexbuf,digits lexbuf) ;;
(* let first_token = digits lexbuf *)
(* let second_token = digits lexbuf *)
    
type t = [`Euro_sign | `Number of int]

let string_of_t x =
  match x with
  | `Euro_sign -> "euro"
  | `Number x -> string_of_int x
        
let _ = begin 
  prerr_endline (string_of_t first_token);
  prerr_endline (string_of_t second_token);
end
