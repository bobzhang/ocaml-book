open Printf
open Aast
open Camlp4.PreCast
open Util

  
let t,t_eoi = Gram.Entry.mk "t" , Gram.Entry.mk "t_eoi"   
let t_of_string _loc str =
  try
    Gram.parse_string t_eoi _loc str
  with
    Loc.Exc_located(t,exn) -> 
      prerr_endline ^$ Loc.to_string t ;
      raise exn

	
let _ = begin
  EXTEND Gram GLOBAL:t t_eoi ;
  t_eoi:
    [ [x = t; `EOI -> x ] ];
  t:
    [ "obj-array"
      ["{"; kvs = LIST0 [ s=STRING; ":"; v= t -> (s,v) ] SEP "," ;"}" ->
        Jq_object kvs
      |"["; xs = LIST0 SELF SEP "," ; "]" ->
        Jq_array xs ]
    | "simple"
      ["("; x = SELF ; ")" -> x
      |"null"  -> Jq_null
      |"false" -> Jq_bool false
      |"true"  -> Jq_bool true
      | `FLOAT(f,_) -> Jq_number f 
      | x = STRING -> Jq_string x ]	
    ];
  END;
end 


let a =
  <:here<
{ "a": true ,
  "b" : false ,
   "c" : {"a" : [true,false] }}
>> |> t_of_string Loc.ghost |> print_t 
    

let _ =
  <:json<
{ "a": true ,
  "b" : false ,
   "c" : {"a" : [true,false] }} >>  |> print_t ;;










