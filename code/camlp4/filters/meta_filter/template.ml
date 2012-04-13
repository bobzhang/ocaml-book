open Camlp4.PreCast;

value expr_basic_template  _loc =
    <:str_item<
      value meta_string _loc s = <:expr< $str:s $ >>;
      value meta_int _loc i =  <:expr< $`int:i$ >>;
      value meta_float _loc f = <:expr< $`flo:f$ >>;
      value meta_char _loc s = <:expr< $`chr:s$ >>;
      value meta_bool _loc = fun
	    [ False -> <:expr< False >>
	    | True -> <:expr< True >> ] ;
      value rec meta_list mf_a _loc = fun
	    [ [] -> <:expr< [] >>
	    | [x::xs] ->
	      <:expr< [ $mf_a _loc x$ :: $meta_list mf_a _loc xs$ ] >> ];
      >>
;

value patt_basic_template  _loc =
    <:str_item<
      value meta_string _loc s = <:patt< $str:s $ >> ;
      value meta_int _loc i =  <:patt< $`int:i$ >>   ;
      value meta_float _loc f = <:patt< $`flo:f$ >>   ;
      value meta_char _loc s = <:patt< $`chr:s$ >>   ;
      value meta_bool _loc = fun
	    [ False -> <:patt< False >>
	    | True -> <:patt< True >> ]     ;
      value rec meta_list mf_a _loc = fun
	    [ [] -> <:patt< [] >>
	    | [x::xs] ->
	      <:patt< [ $mf_a _loc x$ :: $meta_list mf_a _loc xs$ ] >> ]   ;
      >> 
;




