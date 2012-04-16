open Format
open Camlp4.PreCast
open Camlp4
let abstract = ref true

(** Abstract types are represented with the empty type.
 *)
let _ =
  let open Syntax in begin
    EXTEND Gram  GLOBAL: ctyp;
    ctyp: [[ LIDENT "semi"; LIDENT "opaque"; t = SELF ->
      (* <:ctyp< 's >>TyQuo of Loc.t and string *)
      if !abstract
          
      then <:ctyp< >>
        (* then <:ctyp< 'abstract >> *)
      else t
           ]];
    END;
  end 

let _ =  begin 
  Camlp4.Options.add "-abstract" (Arg.Set abstract)
    "Use abstract types for semi opaque ones";
  Options.add "-concrete" (Arg.Clear abstract)
    "Use concrete types for semi opaque ones";
end 
(**
   camlp4o -I _build pa_abstract.cmo -str 'type t  = semi opaque string' -printer r
 *)

















