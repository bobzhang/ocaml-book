(**AstFilters.ml*)
module Make (Ast : Sig.Camlp4Ast)
: Sig.AstFilters with module Ast = Ast
= struct

  module Ast = Ast;

  type filter 'a = 'a -> 'a;

  value interf_filters = Queue.create ();
  value fold_interf_filters f i = Queue.fold f i interf_filters;
  value implem_filters = Queue.create ();
  value fold_implem_filters f i = Queue.fold f i implem_filters;
  value topphrase_filters = Queue.create ();
  value fold_topphrase_filters f i = Queue.fold f i topphrase_filters;

  value register_sig_item_filter f = Queue.add f interf_filters;
  value register_str_item_filter f = Queue.add f implem_filters;
  value register_topphrase_filter f = Queue.add f topphrase_filters;
end;
