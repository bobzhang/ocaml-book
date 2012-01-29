open Camlp4.PreCast
open Syntax

let fresh = let r = ref 0 in fun prefix -> incr r; prefix ^ string_of_int !r

(* raise when we can't infer finalizer's name *)
exception Explicit_finializer

let contains s s' = 
  let len = String.length s and len' = String.length s' in
  if len < len' then None else
    let found = ref None in
    let _ = try
      for i = 0 to len - len' do
        let j = ref 0 in
        while !j < len' && s.[i + !j] = s'.[!j] do incr j done;
        if !j >= len' then (found := Some i; raise Exit);
      done
    with Exit -> () in
    !found

let antonyms = ["open", "close", ["in"; "out"];
                "lock", "unlock", []]

let guess = 
  let try_get_neg (pos, neg, delims) s =
    let len = String.length s in
    let poslen = String.length pos in
    if not (len >= poslen && String.sub s 0 poslen = pos) then None
    else 
      let pos_body = String.sub s poslen (len - poslen) in
      let rec get_neg_body = function
        | [] -> pos_body
        | h::t -> match contains pos_body ("_"^h^"_") with
          | Some i -> String.sub pos_body 0 i ^ ("_"^h)
          | None -> get_neg_body t in
      Some (neg ^ get_neg_body delims) in
  let rec get_neg s = function
    | [] -> None
    | h::t -> match try_get_neg h s with
      | None -> get_neg s t
      | res -> res in
  fun s -> get_neg s antonyms


let rec infer_finalizer =
  let rec unfold_id l = function
    | <:ident< $h$.$t$ >> -> unfold_id (h::l) t
    | <:ident< $lid:lid$ >> -> l, lid
    | _ -> raise Explicit_finializer in
  let fold_id _loc pids lid = 
    List.fold_left (fun t h -> <:ident<$h$.$t$>>) <:ident<$lid:lid$>> pids in
  fun _loc -> function
  | <:expr< $e1$ $_$ >> | <:expr< $e1$; $_$ >> -> infer_finalizer _loc e1
  | <:expr< $id:fid$ >> -> 
    let pids, lid = unfold_id [] fid in
    (match guess lid with
     | Some nlid -> Some (fold_id _loc pids nlid)
     | _ -> None)
  | _ -> None

let use_expr _loc r bi optf body =
  let rsrc = fresh "rsrc" in
  let patt, expr, finalizer = match optf, bi with
    | Some f, <:binding< $p$ = $e$ >> -> p, e, f
    | None, <:binding< $p$ = $e$>> -> begin match infer_finalizer _loc e with
      | Some f -> p, e, <:expr< $id:f$ >> | None -> raise Explicit_finializer end
    | _ -> raise Explicit_finializer in
  <:expr<
    let $rec:r$ ($patt$ as $lid:rsrc$) = $expr$ in
    try
      let result = $body$ in
      let _ = $finalizer$ $lid:rsrc$ in
      result
    with e -> let _ = $finalizer$ $lid:rsrc$ in raise e
  >> 

EXTEND Gram
GLOBAL: expr;
  expr: LEVEL "top"
    [ [ "use"; r = opt_rec; bi = binding; 
        optf = OPT ["with"; f = expr -> f];
        "in"; body = expr LEVEL ";" -> use_expr _loc r bi optf body ] ]
;
END

