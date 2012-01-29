let _ = 
  let x = 1 in 3 + x
let _ = 
  let x = 1 in 4 + x 	  
(**

(setglobal Test!
  (seq
   (let (x/1030 1) (+ 3 x/1030))
   (let (x/1031 1) (+ 4 x/1031))
    (makeblock 0)))
*)  
