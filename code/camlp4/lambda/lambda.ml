open Format
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
        
        
let lambda = Gram.Entry.mk "lambda"
let lambda_eoi = Gram.Entry.mk "lambda_eoi"
    
let _ = begin
  EXTEND Gram GLOBAL: lambda lambda_eoi;
  lambda_eoi:
    [ [x = lambda ; `EOI -> x ] ];
  lambda:
    [
     "top" (** str antiquot -> string literal *)
       [ "fun"; x=LIDENT; "->" ; y=SELF ->
         <:expr< `Lambda ( $ <:expr< `Id $str:x$ >> $, $y$ ) >>  ]
  | "app"
      [x=SELF;y=SELF -> <:expr< `App ($x$,$y$ ) >> ]
  |"simple"
      [ "("; x = SELF; ")" -> x
      | x = LIDENT ->  <:expr< `Id $str:x$ >> ]
];
  END;
end
    
let parse_lambda = parser_of_entry lambda
let parse_lambda_eoi = parser_of_entry lambda_eoi


let mk_quotation ~entry ~tag_name ()=   
  let expand_expr _loc _loc_name_opt quotation_contents  =
    let open Camlp4_config in
    let old = !antiquotations in
    let ()  = antiquotations := true in
    let res = Gram.parse_string entry _loc quotation_contents in
    let ()  = antiquotations := old in
    let () = 
      match _loc_name_opt with
      |None -> print_endline "None"
      |Some s -> print_endline (sprintf "Some %s" s) in 
    res in 
  let expand_str_item _loc _loc_name_opt quotation_contents  =
    let open Camlp4_config in
    let old = !antiquotations in
    let () = antiquotations := true in
    let res = Gram.parse_string entry _loc quotation_contents in
    let () = antiquotations := old in
    <:str_item< $exp:res$ >> in 
  let open Syntax.Quotation in begin
    add tag_name DynAst.expr_tag expand_expr;
    add tag_name DynAst.str_item_tag expand_str_item;
  end


let () = 
  mk_quotation ~entry:lambda_eoi ~tag_name:"lam"  ()



















