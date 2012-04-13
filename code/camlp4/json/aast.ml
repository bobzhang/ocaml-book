
open Sexplib;
open Sexplib.Std;
open Util;

type t =
  [ Jq_null
  | Jq_bool   of bool
  | Jq_string of string
  | Jq_array  of list t
  | Jq_number of float 
  | Jq_object of list (string * t)
]
with sexp;



value string_of_t v = do {
 let buf = Buffer.create 30 ;
 let open Format in
 let fmt = formatter_of_buffer buf ;
 v |> sexp_of_t |> Sexp.pp_hum fmt;
 Buffer.contents buf   
};

value print_t v = v |> string_of_t |> print_string ;

