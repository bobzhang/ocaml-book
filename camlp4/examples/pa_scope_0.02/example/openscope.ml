let ni = Unix @ {ni_hostname = "name"; ni_service = "service"}

let numerate l = 
  Array @ let a = of_list l in
  List @ combine (to_list (mapi (fun i _ -> i + 1) a)) l

let uniq sl = List @ Set.Make(String) @ 
  let of_list l = fold_right add l empty in
  let to_list s = rev (fold (fun x l -> x :: l) s []) in
  to_list (of_list sl)
;;


