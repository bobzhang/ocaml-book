
module Id = struct
  value name = "exception_wrapper";
  value version = Sys.ocaml_version;
end ;

module Make (AstFilters : Camlp4.Sig.AstFilters) = struct
  open AstFilters;
  open Ast;
  value add_tracer e =
    let _loc = Loc.make_absolute (Ast.loc_of_expr e) in
    let msg = "Exception tracer at " ^ Loc.to_string _loc ^ " (%s)@." in
    <:expr<
      try $e$
      with
        [ exc -> do {
           Format.eprintf $str:msg$ (Printexc.to_string exc);
           raise exc;
          }
        ] >> ;

  value match_case = fun
    [ <:match_case@_loc< $_$ when $ <:expr< >> $ -> $_$ >> as x   ->
      (* <:match_case< $p$ when $e1$ -> $add_tracer e2$ >> *)
    | <:match_case< $_$ | $_$ >> as x -> x
    | McNil _ as x -> x           
    | McAnt _ _ -> assert False (** McAnt should not appear in the toplevel *)
    ];

  value map_item ?(str_item= fun x -> x) ?(expr=fun x -> x) () =
  object
    inherit map as super;
    method! str_item = fun x -> str_item (super#str_item x);
    method! expr = fun x -> expr (super#expr x);
    method! match_case = fun x -> match_case (super#match_case x);
  end;

  register_str_item_filter
    (map_item  () )#str_item;
end;


let module M = Camlp4.Register.AstFilter Id Make in ();
