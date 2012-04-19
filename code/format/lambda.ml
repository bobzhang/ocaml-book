open Format
let rec pp_list_aux sep pp_element ppf = function
  | [h] -> fprintf ppf "%a" pp_element h
  | h::t -> begin
      fprintf ppf "%a%s@,%a" pp_element h sep (pp_list_aux sep pp_element) t 
  end 
  | [] ->()

let pp_list ?(sep=",") pp_element ppf xs  =
  fprintf ppf "@[<v 1>[@,%a@,]@]" (pp_list_aux sep pp_element) xs        

let pretty_decl ppf (name,l) =
  fprintf ppf "@[<hv 4>%s{" name;
  List.iter (fun instr -> fprintf ppf "@ %s;" instr) l;
  fprintf ppf "@;<1 -4>}@]"

let _ = pretty_decl std_formatter ("haha",
                                   ["a very long name";
                                    "b v shogsohgso ohgos";
                                    "b v shogsohgso ohgos";                                  
                                    "c";
                                    "b v shogsohgso ohgos";                                                                    ])    
