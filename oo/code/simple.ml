let poly = object
  val vertices = [|0,0;1,1;2,2|]
  method draw = "test"
end 
(**
   val poly : < draw : string > = <obj>   
*)


let draw_list  =   List.iter (fun x -> x#draw)
(**
   val draw_list : < draw : unit; _.. > list -> unit = <fun>
*)


type 'a blob = <draw : unit;  ..> as 'a 
(* type 'a blob = 'a constraint 'a = < draw : unit; .. >   *)

type 'a blob = 'a constraint 'a = < draw : unit > ;;
(* type 'a blob = 'a constraint 'a = < draw : unit >   *)

type 'a blob = 'a constraint 'a = < draw : unit ; .. > ;;
(* type 'a blob = 'a constraint 'a = < draw : unit; .. > *)


let transform = 
    object 
      val matrix = (1.,0.,0.,0.,1.,0.)
      method new_scale sx sy =
        {<matrix= (sx,0.,0.,0.,sy,0.)>}
      method new_rotate theta = 
        let s,c=sin theta, cos theta in 
        {<matrix=(c,-.s,0.,s,c,0.)>}
      method new_translate dx dy=
        {<matrix=(1.,0.,dx,0.,1.,dy)>}
      method transform (x,y) = 
        let (m11,m12,m13,m21,m22,m23)=matrix in 
        (m11 *. x +. m12 *. y +. m13,
         m21 *. x +. m22 *. y +. m23)
    end ;;

(**
  val transform :
  < new_rotate : float -> 'a; new_scale : float -> float -> 'a;
    new_translate : float -> float -> 'a;
    transform : float * float -> float * float >
  as 'a = <obj>
*)


let new_collection () =  object 
  val mutable items = [] 
  method add item = items <- item::items
  method transform mat = 
    {<items = List.map (fun item -> item#transform mat) items>}
end ;;

(*
val new_collection :
  unit ->
  (< add : (< transform : 'c -> 'b; .. > as 'b) -> unit;
     transform : 'c -> 'a >
   as 'a) =
  <fun>
*)

let test_init =object 
    val x = 1 
    val mutable x_plus_1 = 0 
    initializer begin 
      print_endline "hello ";
      x_plus_1 <- x + 1;
    end 
end ;;

(**
hello 
val test_init : <  > = <obj>
*)


let test_private = object
  val x = 1 
  method private print =
    print_int x
end ;;
(* val test_private : <  > = <obj> *)
