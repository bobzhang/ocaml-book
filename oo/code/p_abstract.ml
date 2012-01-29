open Pcaml;;
let abstract = ref true;;
EXTEND
ctyp: [[ LIDENT "semi"; LIDENT "opaque"; t = SELF ->
 if !abstract then <:ctyp< 'abstract >> else t
       ]];
END;;

Pcaml.add_option "-abstract" (Arg.Set abstract)
 "Use abstract types for semi opaque ones";;
Pcaml.add_option "-concrete" (Arg.Clear abstract)
 "Use concrete types for semi opaque ones";; 
