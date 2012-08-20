open Camlp4.PreCast 

(* Extend or modify the ast of an implementation. 
 * 
 * val extend_ast : Ast.str_item -> Ast.str_item 
 *) 
let extend_ast str_item = 
  let _loc = Loc.ghost in 
  <:str_item< open Bla ; $str_item$ >> 


(* Implementation parser to be used instead of the default one. To 
 * be registered with Register.register_str_item_parser. 
 * 
 * my_parse_implem dir_handler start_loc char_stream  parses char_stream 
 * with the default parser from the PreCast.Syntax module. Argument 
 * start_loc is the initial location obtained for example by Loc.mk. 
 * Argument dir_handler is called whenever a top-level directive is 
 * encountered in the char_stream. 
 * 
 * val my_parse_implem : 
 *   ?directive_handler:(Syntax.Ast.str_item -> Syntax.Ast.str_item option) -> 
 *   Syntax.Ast.loc -> char Stream.t -> Ast.str_item 
 *) 
let my_parse_implem ?(directive_handler = fun _ -> None) _loc cs = 
  let str_item = Syntax.parse_implem ~directive_handler _loc cs in 
  extend_ast str_item 
