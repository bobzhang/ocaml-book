
let p = object
  val mutable x = 0
  method get_x = x
  method move d = x <- x + d 
end

(**
(setglobal Oo! (* module name *)
  (let
    (shared/1057 [0: #"move" #"get_x"]
     p/1030
       (let
         (class/1049 (apply (field 15 (global CamlinternalOO!)) shared/1057)
          obj_init/1061
            (let
              (ids/1055
                 (apply (field 3 (global CamlinternalOO!)) class/1049
                   shared/1057 [0: #"x"])
               move/1035 (field 0 ids/1055)
               get_x/1034 (field 1 ids/1055)
               x/1033 (field 2 ids/1055))
              (seq
                (apply (field 10 (global CamlinternalOO!)) class/1049
                  (makeblock 0 get_x/1034 1a x/1033 move/1035
                    (function self-1/1039 d/1040
                      (array.unsafe_set self-1/1039 x/1033
                        (+ (array.unsafe_get self-1/1039 x/1033) d/1040)))))
                (function env/1051
                  (let
                    (self/1052
                       (apply (field 23 (global CamlinternalOO!)) 0a
                         class/1049))
                    (seq (array.unsafe_set self/1052 x/1033 0) self/1052))))))
         (seq (apply (field 16 (global CamlinternalOO!)) class/1049)
           (apply obj_init/1061 0a))))
    (makeblock 0 p/1030)))
   
*)  
(**
let shared = [|"move";"get_x"|] 
let p = 
  let clas = OO.create_table shared in 
  let obj_init = 
    let ids = OO.new_methods_variables clas shared [|"x"|] in 
    let move = ids.(0) in 
    let get_x = ids.(1) in 
    let x = ids.(2) in 
    OO.set_methods clas [| 
      get_x; OO.GetVar; x; 
      move; (fun self d -> self.(x) <- self.(x) + d); 
    |]; 
    (fun env -> 
       let self = OO.create_object_opt 0 clas in 
       self.(x) <- 0; 
       self) in 
  OO.init_class clas; 
  obj_init 0 
*)
