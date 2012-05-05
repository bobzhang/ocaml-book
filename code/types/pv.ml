open Formatf 

let classify_chars s =
  let rec classify_chars_at p =
    if p < String.length s then
      let c = s.[p] in
      let cls =
	match c with
	  | '0' .. '9' -> `Digit c
	  | 'A' .. 'Z' | 'a' .. 'z' -> `Letter c
	  | _ -> `Other c in
      cls :: classify_chars_at (p+1)
    else []
  in
  classify_chars_at 0
(* val classify_chars :
  string -> [> `Digit of char | `Letter of char | `Other of char ] list 
*)    


let recognize_numbers l =
  let rec recognize_at m acc =
    match m with
      | `Digit d :: m' ->
        let d_v = Char.code d - Char.code '0' in
        let acc' =
          match acc with
            | Some v -> Some(10*v + d_v)
            | None -> Some d_v in
        recognize_at m' acc'
      (** here makes the input and output the same time *)	  
      | x :: m' ->
        ( match acc with
          | None -> x :: recognize_at m' None
          | Some v -> (`Number v) :: x :: recognize_at m' None
        )
      | [] ->
        ( match acc with
          | None -> []
          | Some v -> (`Number v) :: []
        )
  in
  recognize_at l None
(** val recognize_numbers :
    ([> `Digit of char | `Number of int ] as 'a ) list -> 'a list

    Note that the type of the recognize_numbers
    function does not reflect all what we could know about the
    function. We can be sure that the function will never return a
    `Digit tag, but this is not expressed in the function type. We have
    run into one of the cases where the O'Caml type system is not
    powerful enough to find this out, or even to write this knowledge
    down. In practice, this is no real limitation - the types are
    usually a bit weaker than necessary, but it is unlikely that weaker
    types cause problems.'
*)
    

let analysis = classify_chars |- recognize_numbers
(**
   val analysis:
     string ->
     [> `Digit of char | `Letter of char | `Number of int | `Other of char ]  list

   It is no problem that classify_chars emits tags that are completely
   unknown to recognize_numbers. And both functions can use the same
   tag, `Digit, without having to declare in some way that they are
   meaning the same. It is sufficient that the tag is the same, and
   that the attached value has the same type.
*)


let number_value1 t =
  match t with
    | `Number n -> n
    | `Digit d -> Char.code d - Char.code '0'

let number_value2 t =
  match t with
    | `Number n -> n
    | `Digit d -> Char.code d - Char.code '0'
    | _ -> failwith "This is not a number"

(**
   val number_value1 : [< `Digit of char | `Number of int ] -> int = <fun>
   val number_value2 : [> `Digit of char | `Number of int ] -> int = <fun>
*)

type classified_char =
  [ `Digit of char | `Letter of char | `Other of char ]

type number_token =
  [ `Digit of char | `Number of int ]

(**
   Note that there is no ">" or "<" sign in such definitions - it
   would not make sense to say something about whether more or less
   tags are possible than given, because the context is missing.
*)

type classified_number_token = [classified_char  | number_token ]
(**
   type classified_number_token =
    [ `Digit of char | `Letter of char | `Number of int | `Other of char ]
*)

let rec sum l =
  match l with
    | x :: l' ->
      ( match x with
        | `Digit _ | `Number _ ->
              number_value1 x + sum l'
        | _ ->
              sum l' )
    | [] -> 
      0
(**
   Warning 11: this match case is unused.
   val sum : [ `Digit of char | `Number of int ] list -> int = <fun>
*)

let rec sum2 l =
  match l with
    | x :: l' ->
      ( match x with
        | (`Digit _ | `Number _) as y ->
              number_value1 y + sum2 l'
        | _ ->
              sum2 l'
      )
    | [] -> 
      0
(**
val sum2 : [> `Digit of char | `Number of int ] list -> int = <fun>   
*)
let rec sum3 l =
  match l with
    | #number_token as y :: l' ->
      number_value1 y + sum3 l'
    | _ :: l' -> sum3 l'
    | [] -> 0
      
(**
 val sum3 : [> number_token ] list -> int = <fun>
*)

(** Internally, the tags are represented by hash values of the names
    of the tags. So the tags are simply reduced to integers at
    runtime. Compared with the normal variant types, there is some
    additional overhead for tags with values. In particular, for
    storing `X value one extra word is needed in comparison with the
    normal variant X value.  *)




