open Camlp4.PreCast
  
let parser_of_entry entry  s =
  try Gram.parse entry (Loc.mk "<string>") (Stream.of_string  s)
  with
    Loc.Exc_located(loc, e) -> begin 
      prerr_endline (Loc.to_string loc);
      let start_bol,stop_bol,
        start_off, stop_off =
        Loc.(start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc
            ) in
      let abs_start_off = start_bol + start_off in
      let abs_stop_off = stop_bol + stop_off in
      let err_location = String.sub s abs_start_off
          (abs_stop_off - abs_start_off + 1) in
      prerr_endline (sprintf "err: ^%s^" err_location);
      raise e ;
    end
        
        
let expr = Gram.Entry.mk "expr"
let expr_eoi = Gram.Entry.mk "expr_eoi"
    
let _ = begin
  EXTEND Gram GLOBAL: expr expr_eoi;
  expr_eoi:
    [ [x = expr ; `EOI -> x ] ];
  expr:
    [
     "add"
       [  x = SELF ; "+"; y = SELF -> x + y
       |  x = SELF ; "-"; y = SELF -> x - y]
    |"mult"
       [  x = SELF ; "*"; y = SELF -> x * y
       |  x = SELF ; "/"; y = SELF -> x / y]
    | "pow" RIGHTA
       [ x = SELF ; "**"; y = SELF -> int_of_float (float x ** float y)] 
    |"simple"
      [ "("; x = SELF; ")" -> x
      | `INT(x,_) -> x ]
];
  END;
end
    
let parse_expr = parser_of_entry expr
let parse_expr_eoi = parser_of_entry expr_eoi
