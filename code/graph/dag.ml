open Format
open Graph.Pack.Digraph

let g = Rand.graph ~v:10 ~e:20 ()

let _ = dot_output g "g.dot"

(** get a transitive_closure, with reflexive true *)    
let g_closure = transitive_closure ~reflexive:true g
let _ = dot_output g_closure "g_closure.dot"

(** get a mirror graph : 0-> 7 ==> 7 -> 0*)    
let g_mirror = mirror g
let _ = dot_output g_mirror "g_mirror.dot"    


    
let g1 = create ()
let g2 = create ()
let [v1;v2;v3;v4;v5;v6;v7 ] = List.map V.create [1;2;3;4;5;6;7]
let _ = ( begin
  add_edge g1 v1 v2;
  add_edge g1 v2 v1;
  add_edge g1 v1 v3;
  add_edge g1 v2 v3;
  add_edge g1 v5 v3;
  add_edge g1 v6 v6;
  add_vertex g1 v4
  end 
)

let _ = ( begin
  add_edge g2 v1 v2;
  add_edge g2 v2 v3;
  add_edge g2 v1 v4;
  add_edge g2 v3 v6;
  add_vertex g2 v7
end
)

(** do intersection *)
let g_intersect = intersect g1 g2
(** do union *)    
let g_union = union g1 g2 

let _ = 
  (
   let f = dot_output in begin
     f g1 "g1.dot";
     f g2 "g2.dot";
     f g_intersect "g_intersect.dot";
     f g_union "g_union.dot";
     Dfs.iter (fun i -> print_int (V.label i); print_newline())  g_union ();
     print_endline "DFS";
     Dfs.iter ~pre:(fun i -> print_int (V.label i);print_newline () ) g_union;
   end
     
  )



















