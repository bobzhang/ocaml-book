open Format

type flt_temp = [ `FLT_TEMP of int]
(* Floating point temporaries *)

type vec_temp = [ `VEC_TEMP of int]

type any_temp = [flt_temp | vec_temp]

(**
   flt_temp :> any_temp
   vec_temp :> any_temp
   operation :> gen_operation
 *)
let any1 : any_temp -> string = function
  | `FLT_TEMP x -> sprintf "t%d:float" x
  | `VEC_TEMP x -> sprintf "t%d:vector" x

let any2 : [<any_temp] ->string = function       
  | `FLT_TEMP x -> sprintf "t%d:float" x
  | `VEC_TEMP x -> sprintf "t%d:vector" x

type operation  = [`DIVISION of flt_temp * flt_temp
                  |`DOT_PRODUCT of vec_temp * vec_temp ]

let e2 : operation -> any_temp list = function
  | `DIVISION(a,b) ->
      [ (a:>any_temp)   ; (b:>any_temp) ]
  | `DOT_PRODUCT(a,b) ->
      [ (a:>any_temp)   ; (b:>any_temp) ]

type gen_operation = [`DIVISION of any_temp * any_temp
                     |`DOT_PRODUCT of any_temp * any_temp ]

      
let e3 (x:operation) = match (x:operation :> gen_operation) with
 | `DOT_PRODUCT(a,b) | `DIVISION(a,b) -> [a;b]

       













