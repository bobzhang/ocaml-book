open Camlp4.PreCast
open Camlp4

let abstract = ref true
let _ = begin
  EXTEND Gram  ctyp: [[ LIDENT "semi"; LIDENT "opaque"; t = SELF ->
    if !abstract then <:ctyp< 'abstract >> else t
       ]];
  END;
end 

let _ =  begin 
  Options.add "-abstract" (Arg.Set abstract)
 "Use abstract types for semi opaque ones";
  Options.add_option "-concrete" (Arg.Clear abstract)
 "Use concrete types for semi opaque ones";
end 
