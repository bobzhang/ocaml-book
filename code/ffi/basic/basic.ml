(* -*- Mode:Tuareg; -*-
   *===----------------------------------------------------------------------===
   * Version: $Id: basisc.ml,v 0.0 2012/02/23 04:45:00 bobzhang1988 Exp $
   *===----------------------------------------------------------------------===*)
open Printf

external inspect_float_array : float array -> unit = "inspect_float_array"  
external inspect_int_array : int array -> unit = "inspect_int_array"  
external inspect_string_list : string list -> unit = "inspect_string_list"
external create_list : string ->  char list = "create_list"

external create_float_array : int -> float-> float array = "create_float_array"
let (|>) x f = f x
external get_raw_data : unit -> string = "get_raw_data"
external get_raw_data2 : unit -> string = "get_raw_data2"  

type t
external abs_get : float -> int -> char -> t = "wrapping_ptr_ml2c"
external print_t : t -> unit = "dump_ptr"
external free_t : t -> unit = "free_ptr"

    
let _ = begin
  (* let is = Array.init 32 (fun i -> i) in is |> inspect_int_array ; *)
  (* let fs= Array.init 32 (fun i-> float i) in fs  |>   inspect_float_array ; *)
  (* ["ahgo";"b";"agh\n\000ags"] |> inspect_string_list; *)
  (* "ghsogho" |> create_list |> List.iter print_char *)
  (* create_float_array 32 2.0 |> Array.iter print_float; *)
  (* get_raw_data2 () |> print_string; *)
  let a = abs_get 1. 1 'c' in begin 
    print_t  a ;
    free_t a ;
    (* free_t a; *)
  end 
end 
















