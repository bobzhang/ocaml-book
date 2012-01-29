
open Camlp4.PreCast
let test =
  __LOCATION__


DEFINE F(x,y,z) = (x + y * z)

let a = F(3,2,4)

(**
camlp4of test_pa_macro.ml -printer o   
open Camlp4.PreCast
  
let test = Loc.of_tuple ("test_pa_macro.ml", 4, 32, 34, 4, 32, 46, false)
  
let a = 3 + (2 * 4)
  


*)  
