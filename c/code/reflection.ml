
open Printf

let margin n stdout = String.(
  init n (fun _ ->' ') |> print stdout
)

let opaque name = "<" ^ name ^ ">"

let view Obj_list = fun t -> Obj.(
  let rec is_list r acc =
    if is_int r
    then if repr 0 = r
      then Some (List.rev  acc)
      else None
    else
      let s = size r in
      let t = tag r in
      if t = 0 && s = 2
      then
	is_list (field r 1) (field r 0 :: acc)
      else None 
  in
  is_list t []
)

(** tag 0, sum type *)    
let rec get_fields t = Obj.(
  let s = size t in
  let v = Array.create s t in 
  for i = 0 to s - 1 do
    v.(i)<-field t i
  done ;
  v
)


let view Obj_obj = fun t -> Obj.(
  match Array.to_list (get_fields t) with
    | h::h'::t -> h,h',t
    | _ -> assert false
)

let view Obj_lazy = fun t -> Obj.(
  if tag t = lazy_tag
  then Some (opaque "lazy")
  else None
)
  
let view Obj_closure = fun t -> Obj.(
  if tag t = closure_tag
  then
    Some(get_fields t )
  else None
)

let view Obj_string = fun t -> Obj.(
  if tag t = string_tag
  then
    Some ("\"" ^
	     (String.escaped (magic t)) ^
	     (* (String.escaped (magic (field t 0 )) : string) ^ *)
	     "\"")
  else None
)

let view Obj_double = fun t -> Obj.(
  if tag t = double_tag
  then Some (string_of_float (magic t : float))
  else None)
let view Obj_long = fun t -> Obj.(
  if tag t = int_tag
  then Some (string_of_int (magic t ))
  else None
)
  
let view Obj_abstract = fun t -> Obj.(
  if tag t = abstract_tag
  then Some (opaque "abstract")
  else None
)
let view Obj_custom = fun t -> Obj.(
  if tag t = custom_tag
  then Some (opaque "custom")
  else None
)
let view Obj_double_array = fun t -> Obj.(
  if tag t = double_array_tag
  then Some (IO.to_string (Array.print Float.print)
	       (magic t : float array))
  else  None 
)

let view Obj_infix = fun t -> Obj.(
  if tag t = infix_tag
  then Some (opaque "infix")
  else None
)
let view Obj_forward = fun t -> Obj.(
  if tag t = forward_tag
  then Some (opaque "forward")
  else None
)

let view Obj_scan = fun t -> Obj.(
  if tag t < no_scan_tag
  then if tag t = 0 
    then Some (`Structure (Array.to_list (get_fields t)))
    else Some (`Scan (Array.to_list(get_fields t)))
  else None
)

(**
   FIXME String.print should print string?
*)	  

let rec obj_print ident stdout   = Obj.(fun r->
  match  r with
    | %Obj_long(s) -> begin
      margin ident stdout;
      String.print stdout s
    end 
    | %Obj_list(fields)->
      margin ident stdout;
      List.print ~first:"[\n" ~sep:";\n" 
	(obj_print (ident+4))
	stdout fields
    | %Obj_closure(s) ->
      (
	match Array.to_list s with
	  | code::args -> begin
	    margin ident stdout;
	    output_string stdout (sprintf "<closure>@%d\n" (magic code));
	    margin ident stdout;
	    List.print
	      ~first:"(\n"
	      ~sep:"\n"
	      ~last:")"
	      (obj_print (ident+4)) stdout args
	  end 
	  
	| _ -> assert false)

    | (
        %Obj_string(s)
      | %Obj_lazy(s)
      | %Obj_string(s)
      | %Obj_double(s)
      | %Obj_abstract(s)
      | %Obj_custom(s)
      | %Obj_double_array(s)
      | %Obj_infix(s)
      | %Obj_forward(s) )
      -> begin
	margin ident stdout;
	String.print stdout s
      end
    | %Obj_scan(v)-> begin
      margin ident stdout;
      let lst = (match v with
	|`Structure lst -> 
	  output_string stdout (sprintf "<structure value>:\n");
	  lst
	|`Scan lst ->
	  output_string stdout (sprintf "<scan value>:\n");
	  lst) in 
      margin ident stdout;
      List.print ~first:"(\n" ~sep:"," ~last:")"
	(obj_print (ident+4)) stdout lst
    end
      
    | _ -> begin
      margin ident stdout;
      String.print stdout "<unknown tag>"
    end 
)

(** FIXME
    Array.print ident needed
    Format module ?
*)
  
let gen_print  x = Obj.(x |> repr |> obj_print 0 stdout)

let _ = begin 
  gen_print [1;2;3];
  gen_print [1.0;2.0;3.0]
end 

let p v   =  v |> gen_print  |> print_newline


(** Arrays,tuples,records => structures *)
type foo = {ld1:int;ld2:int}
type mfoo = {fld1:int; mutable fld2:int}
type foo2 = {d1 : foo; d2 :int}
let test () = begin
  p [|1;2;3|];
  p (10,true,());
  p {ld1=1;ld2=3};
  p {fld1=1;fld2=3};
  p {d1 = {ld1=1;ld2=3}; d2=3};
end


type foo1 =
    C1 of int * int * int (* tag 0*)
  | C2 of int (* tag 1*)
  | C3
  | C4 of int * int (* tag 2*)
let test_sum () = begin
  p (C1 (1,2,3));
  p (C2  3);
  p C3;
  p (C4 (1,2))
end 

let test_float_array () = begin
  p [|1.0;2.0;3.0|]
end

let _ =
  assert  (Obj.(no_scan_tag < ([|1.0|] |> repr |> tag)));
  assert (Obj.( abstract_tag = no_scan_tag &&
		  no_scan_tag < custom_tag
  ))
    
let rec len = function
  |[]-> 0
  |_::t -> 1 + len t

let test_closure () = begin
  let f = fun x y z -> x + y + z in
  p f ;
  (** parital evaluation happens
      x is evaluated here, so
      for closure, it only store immediate values
  *)
  let g = let x = 1+ 2 and y =2 in fun z -> x + y +z in
  p g ;
  let a1 = f 1 in
  p a1;
  let a2 = f 1 2 in
  p a2;
  let h = fun x y z -> x + y + z in
  let a3 = h 1 in
  p a3;
  let g x =
    let y = 2 in
    fun z -> x + y + z in
  p g ;
  let a1 = g 1 in
  p a1;
end 
    
