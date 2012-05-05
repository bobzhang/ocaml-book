open Format

type value = [`Scalar of float | `Vector of float * float ]

type scalar = [`Scalar of float]
      
type vector = [`Vector of float * float ]

type operation =
    [`Multiply of scalar * scalar
    | `DotProduct of vector * vector ]

type general_operation =
    [ `Multiply of value * value 
    | `DotProduct of value * value ]

let values x =
  match (x : operation :> general_operation)  with
  | `Multiply (a,b) ->  [a;b]
  | `DotProduct (a,b) -> [a;b]      


















