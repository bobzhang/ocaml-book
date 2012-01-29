open Camlp4.PreCast
  
open Json_ast
  
module Camlp4TrashX =
  struct
    (**
   camlp4of -filter map -filter fold -filter trash  -filter meta -parser Pa_type_conv.cma pa_sexp_conv.cma pa_json_ast.ml -printer o
*)
    open Sexplib.Std
      
    type float' =
      float
      and t =
      | Jq_null
      | Jq_bool of bool
      | Jq_number of float'
      | Jq_string of string
      | Jq_array of t list
      | Jq_object of (string * t) list
    
    let rec float'_of_sexp__ =
      let _tp_loc = "pa_json_ast.ml.Camlp4TrashX.float'"
      in fun t -> float_of_sexp t
    and float'_of_sexp sexp =
      try float'_of_sexp__ sexp
      with
      | Sexplib.Conv_error.No_variant_match ((_tp_loc, sexp)) ->
          Sexplib.Conv_error.no_matching_variant_found _tp_loc sexp
    and t_of_sexp__ =
      let _tp_loc = "pa_json_ast.ml.Camlp4TrashX.t"
      in
        function
        | Sexplib.Sexp.Atom ("jq_null" | "Jq_null") -> Jq_null
        | (Sexplib.Sexp.List
             (Sexplib.Sexp.Atom (("jq_bool" | "Jq_bool" as _tag)) ::
                sexp_args)
           as _sexp) ->
            (match sexp_args with
             | [ v1 ] -> let v1 = bool_of_sexp v1 in Jq_bool v1
             | _ ->
                 Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
        | (Sexplib.Sexp.List
             (Sexplib.Sexp.Atom (("jq_number" | "Jq_number" as _tag)) ::
                sexp_args)
           as _sexp) ->
            (match sexp_args with
             | [ v1 ] -> let v1 = float'_of_sexp v1 in Jq_number v1
             | _ ->
                 Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
        | (Sexplib.Sexp.List
             (Sexplib.Sexp.Atom (("jq_string" | "Jq_string" as _tag)) ::
                sexp_args)
           as _sexp) ->
            (match sexp_args with
             | [ v1 ] -> let v1 = string_of_sexp v1 in Jq_string v1
             | _ ->
                 Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
        | (Sexplib.Sexp.List
             (Sexplib.Sexp.Atom (("jq_array" | "Jq_array" as _tag)) ::
                sexp_args)
           as _sexp) ->
            (match sexp_args with
             | [ v1 ] -> let v1 = list_of_sexp t_of_sexp v1 in Jq_array v1
             | _ ->
                 Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
        | (Sexplib.Sexp.List
             (Sexplib.Sexp.Atom (("jq_object" | "Jq_object" as _tag)) ::
                sexp_args)
           as _sexp) ->
            (match sexp_args with
             | [ v1 ] ->
                 let v1 =
                   list_of_sexp
                     (function
                      | Sexplib.Sexp.List ([ v1; v2 ]) ->
                          let v1 = string_of_sexp v1
                          and v2 = t_of_sexp v2
                          in (v1, v2)
                      | sexp ->
                          Sexplib.Conv_error.tuple_of_size_n_expected _tp_loc
                            2 sexp)
                     v1
                 in Jq_object v1
             | _ ->
                 Sexplib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
        | (Sexplib.Sexp.List (Sexplib.Sexp.Atom ("jq_null" | "Jq_null") :: _)
           as sexp) -> Sexplib.Conv_error.stag_no_args _tp_loc sexp
        | (Sexplib.Sexp.Atom ("jq_bool" | "Jq_bool") as sexp) ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        | (Sexplib.Sexp.Atom ("jq_number" | "Jq_number") as sexp) ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        | (Sexplib.Sexp.Atom ("jq_string" | "Jq_string") as sexp) ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        | (Sexplib.Sexp.Atom ("jq_array" | "Jq_array") as sexp) ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        | (Sexplib.Sexp.Atom ("jq_object" | "Jq_object") as sexp) ->
            Sexplib.Conv_error.stag_takes_args _tp_loc sexp
        | (Sexplib.Sexp.List (Sexplib.Sexp.List _ :: _) as sexp) ->
            Sexplib.Conv_error.nested_list_invalid_sum _tp_loc sexp
        | (Sexplib.Sexp.List [] as sexp) ->
            Sexplib.Conv_error.empty_list_invalid_sum _tp_loc sexp
        | sexp -> Sexplib.Conv_error.unexpected_stag _tp_loc sexp
    and t_of_sexp sexp = t_of_sexp__ sexp
      
    let rec sexp_of_float' v = sexp_of_float v
    and sexp_of_t =
      function
      | Jq_null -> Sexplib.Sexp.Atom "Jq_null"
      | Jq_bool v1 ->
          let v1 = sexp_of_bool v1
          in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Jq_bool"; v1 ]
      | Jq_number v1 ->
          let v1 = sexp_of_float' v1
          in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Jq_number"; v1 ]
      | Jq_string v1 ->
          let v1 = sexp_of_string v1
          in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Jq_string"; v1 ]
      | Jq_array v1 ->
          let v1 = sexp_of_list sexp_of_t v1
          in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Jq_array"; v1 ]
      | Jq_object v1 ->
          let v1 =
            sexp_of_list
              (fun (v1, v2) ->
                 let v1 = sexp_of_string v1
                 and v2 = sexp_of_t v2
                 in Sexplib.Sexp.List [ v1; v2 ])
              v1
          in Sexplib.Sexp.List [ Sexplib.Sexp.Atom "Jq_object"; v1 ]
      
  end
  
open Camlp4TrashX
  
class map =
  object ((o : 'self_type))
    method string : string -> string = o#unknown
    method list :
      'a 'a_out. ('self_type -> 'a -> 'a_out) -> 'a list -> 'a_out list =
      fun _f_a ->
        function
        | [] -> []
        | _x :: _x_i1 ->
            let _x = _f_a o _x in
            let _x_i1 = o#list _f_a _x_i1 in _x :: _x_i1
    method float : float -> float = o#unknown
    method bool : bool -> bool = function | false -> false | true -> true
    method t : t -> t =
      function
      | Jq_null -> Jq_null
      | Jq_bool _x -> let _x = o#bool _x in Jq_bool _x
      | Jq_number _x -> let _x = o#float' _x in Jq_number _x
      | Jq_string _x -> let _x = o#string _x in Jq_string _x
      | Jq_array _x -> let _x = o#list (fun o -> o#t) _x in Jq_array _x
      | Jq_object _x ->
          let _x =
            o#list
              (fun o (_x, _x_i1) ->
                 let _x = o#string _x in let _x_i1 = o#t _x_i1 in (_x, _x_i1))
              _x
          in Jq_object _x
    method float' : float' -> float' = o#float
    method unknown : 'a. 'a -> 'a = fun x -> x
  end
  
class fold =
  object ((o : 'self_type))
    method string : string -> 'self_type = o#unknown
    method list :
      'a. ('self_type -> 'a -> 'self_type) -> 'a list -> 'self_type =
      fun _f_a ->
        function
        | [] -> o
        | _x :: _x_i1 -> let o = _f_a o _x in let o = o#list _f_a _x_i1 in o
    method float : float -> 'self_type = o#unknown
    method bool : bool -> 'self_type = function | false -> o | true -> o
    method t : t -> 'self_type =
      function
      | Jq_null -> o
      | Jq_bool _x -> let o = o#bool _x in o
      | Jq_number _x -> let o = o#float' _x in o
      | Jq_string _x -> let o = o#string _x in o
      | Jq_array _x -> let o = o#list (fun o -> o#t) _x in o
      | Jq_object _x ->
          let o =
            o#list
              (fun o (_x, _x_i1) ->
                 let o = o#string _x in let o = o#t _x_i1 in o)
              _x
          in o
    method float' : float' -> 'self_type = o#float
    method unknown : 'a. 'a -> 'self_type = fun _ -> o
  end
  
module MetaExpr =
  struct
    let meta_float' _loc f =
      Ast.ExFlo (_loc, (Camlp4_import.Oprint.float_repres f))
      
    include
      struct
        let meta_string _loc s = Ast.ExStr (_loc, s)
          
        let meta_int _loc s = Ast.ExInt (_loc, s)
          
        let meta_float _loc s = Ast.ExFlo (_loc, s)
          
        let meta_char _loc s = Ast.ExChr (_loc, s)
          
        let meta_bool _loc =
          function
          | false -> Ast.ExId (_loc, (Ast.IdUid (_loc, "False")))
          | true -> Ast.ExId (_loc, (Ast.IdUid (_loc, "True")))
          
        let rec meta_list mf_a _loc =
          function
          | [] -> Ast.ExId (_loc, (Ast.IdUid (_loc, "[]")))
          | x :: xs ->
              Ast.ExApp (_loc,
                (Ast.ExApp (_loc,
                   (Ast.ExId (_loc, (Ast.IdUid (_loc, "::")))),
                   (mf_a _loc x))),
                (meta_list mf_a _loc xs))
          
        let rec meta_t _loc =
          function
          | Camlp4TrashX.Jq_object x0 ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_object")))))),
                (meta_list
                   (fun _loc (x1, x2) ->
                      Ast.ExTup (_loc,
                        (Ast.ExCom (_loc, (meta_string _loc x1),
                           (meta_t _loc x2)))))
                   _loc x0))
          | Camlp4TrashX.Jq_array x0 ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_array")))))),
                (meta_list meta_t _loc x0))
          | Camlp4TrashX.Jq_string x0 ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_string")))))),
                (meta_string _loc x0))
          | Camlp4TrashX.Jq_number x0 ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_number")))))),
                (meta_float' _loc x0))
          | Camlp4TrashX.Jq_bool x0 ->
              Ast.ExApp (_loc,
                (Ast.ExId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_bool")))))),
                (meta_bool _loc x0))
          | Camlp4TrashX.Jq_null ->
              Ast.ExId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                   (Ast.IdUid (_loc, "Jq_null")))))
          
      end
      
  end
  
module MetaPatt =
  struct
    let meta_float' _loc f =
      Ast.PaFlo (_loc, (Camlp4_import.Oprint.float_repres f))
      
    include
      struct
        let meta_string _loc s = Ast.PaStr (_loc, s)
          
        let meta_int _loc s = Ast.PaInt (_loc, s)
          
        let meta_float _loc s = Ast.PaFlo (_loc, s)
          
        let meta_char _loc s = Ast.PaChr (_loc, s)
          
        let meta_bool _loc =
          function
          | false -> Ast.PaId (_loc, (Ast.IdUid (_loc, "False")))
          | true -> Ast.PaId (_loc, (Ast.IdUid (_loc, "True")))
          
        let rec meta_list mf_a _loc =
          function
          | [] -> Ast.PaId (_loc, (Ast.IdUid (_loc, "[]")))
          | x :: xs ->
              Ast.PaApp (_loc,
                (Ast.PaApp (_loc,
                   (Ast.PaId (_loc, (Ast.IdUid (_loc, "::")))),
                   (mf_a _loc x))),
                (meta_list mf_a _loc xs))
          
        let rec meta_t _loc =
          function
          | Camlp4TrashX.Jq_object x0 ->
              Ast.PaApp (_loc,
                (Ast.PaId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_object")))))),
                (meta_list
                   (fun _loc (x1, x2) ->
                      Ast.PaTup (_loc,
                        (Ast.PaCom (_loc, (meta_string _loc x1),
                           (meta_t _loc x2)))))
                   _loc x0))
          | Camlp4TrashX.Jq_array x0 ->
              Ast.PaApp (_loc,
                (Ast.PaId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_array")))))),
                (meta_list meta_t _loc x0))
          | Camlp4TrashX.Jq_string x0 ->
              Ast.PaApp (_loc,
                (Ast.PaId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_string")))))),
                (meta_string _loc x0))
          | Camlp4TrashX.Jq_number x0 ->
              Ast.PaApp (_loc,
                (Ast.PaId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_number")))))),
                (meta_float' _loc x0))
          | Camlp4TrashX.Jq_bool x0 ->
              Ast.PaApp (_loc,
                (Ast.PaId (_loc,
                   (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                      (Ast.IdUid (_loc, "Jq_bool")))))),
                (meta_bool _loc x0))
          | Camlp4TrashX.Jq_null ->
              Ast.PaId (_loc,
                (Ast.IdAcc (_loc, (Ast.IdUid (_loc, "Camlp4TrashX")),
                   (Ast.IdUid (_loc, "Jq_null")))))
          
      end
      
  end
  

