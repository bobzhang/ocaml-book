
(* #default_quotation "lam";;
   error location will be totally mess if you use this.
*)

let id = <:lam< \ x -> x  x >>

let zero = <:lam< \ s -> \ z -> s z >>

let succ  = <:lam< \ n -> \ s -> \ z -> s n >>

let one = <:lam< $succ$ >>

let iota = <:lam< \ x -> z >>  

(** iota interpreted as `Var string *)  
let rho = <:lam< \ m -> \ r -> ( s m (m r iota r )) >>

let rec_nat =
  <:lam< \ n -> \ s -> \ z -> n $rho$ $iota$ $rho$ >>

let plus =
  <:lam< \n -> \m -> $rec_nat$ n ( \ n -> \p -> $succ$ p ) m >>

let times =
  <:lam< \ n -> \ m -> $rec_nat$ n ( \ n -> \ p -> $plus$ m p ) $zero$ >>

let fact =
  <:lam< \ n -> $rec_nat$ n (\ n -> \ p -> $times$ ($succ$ n ) p ) $one$ >>


(**
   (* #default_quotation "lam";;
   error location will be totally mess if you use this.
*)
let id = `Lam ((`Var "x"), (`App ((`Var "x"), (`Var "x"))))
  
let zero =
  `Lam ((`Var "s"), (`Lam ((`Var "z"), (`App ((`Var "s"), (`Var "z"))))))
  
let succ =
  `Lam ((`Var "n"),
    (`Lam ((`Var "s"), (`Lam ((`Var "z"), (`App ((`Var "s"), (`Var "n"))))))))
  
let one = succ
  
let iota = `Lam ((`Var "x"), (`Var "z"))
  
(** iota interpreted as `Var string *)
let rho =
  `Lam ((`Var "m"),
    (`Lam ((`Var "r"),
       (`App ((`App ((`Var "s"), (`Var "m"))),
          (`App ((`App ((`App ((`Var "m"), (`Var "r"))), (`Var "iota"))),
             (`Var "r"))))))))
  
let rec_nat =
  `Lam ((`Var "n"),
    (`Lam ((`Var "s"),
       (`Lam ((`Var "z"),
          (`App ((`App ((`App ((`Var "n"), rho)), iota)), rho)))))))
  
let plus =
  `Lam ((`Var "n"),
    (`Lam ((`Var "m"),
       (`App
          ((`App ((`App (rec_nat, (`Var "n"))),
              (`Lam ((`Var "n"),
                 (`Lam ((`Var "p"), (`App (succ, (`Var "p"))))))))),
          (`Var "m"))))))
  
let times =
  `Lam ((`Var "n"),
    (`Lam ((`Var "m"),
       (`App
          ((`App ((`App (rec_nat, (`Var "n"))),
              (`Lam ((`Var "n"),
                 (`Lam ((`Var "p"),
                    (`App ((`App (plus, (`Var "m"))), (`Var "p"))))))))),
          zero)))))
  
let fact =
  `Lam ((`Var "n"),
    (`App
       ((`App ((`App (rec_nat, (`Var "n"))),
           (`Lam ((`Var "n"),
              (`Lam ((`Var "p"),
                 (`App ((`App (times, (`App (succ, (`Var "n"))))),
                    (`Var "p"))))))))),
       one)))
  


*)  
