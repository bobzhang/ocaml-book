open Format

class ['a] value_ring values = object
  val mutable values = (values : 'a list)
  method get =
    match values with
    | h::t -> values <- t @[h] ; h
    | [] -> raise Not_found
  method add value =
    values <- value :: values
end
    
(* Magic hash that autoappends. *)

class ['a, 'b] append_hash size = object
  val hash = (Hashtbl.create size : ('a, 'b) Hashtbl.t)
  method get k = Hashtbl.find hash k
  method set k v =
    Hashtbl.replace hash k
      (try v :: Hashtbl.find hash k with Not_found -> [v])
  method each f = Hashtbl.iter f hash
end

let () =
  let tab = new append_hash 3 in
  tab#set "beer" "guinness";
  tab#set "food" "potatoes";
  tab#set "food" "peas";
  tab#each
    (fun k vs ->
       Printf.printf "%s => [%s]\n"
         k (String.concat " " (List.rev vs)))    

(** syntax support .... *)
module Bigarray = struct
  module Array1 = struct
    let get obj = obj#get
    let set obj = obj#set
  end
end

let () =
  let tab = new append_hash 3 in
  tab.{"beer"} <- "guinness";
  tab.{"food"} <- "potatoes";
  tab.{"food"} <- "peas";
  tab#each
    (fun k vs ->
      Printf.printf "%s => [%s]\n"
        k (String.concat " " (List.rev vs)));
  print_endline (List.hd tab.{"beer"})


    
class ['a] folded_hash size = object
  val hash = (Hashtbl.create size : (string, 'a) Hashtbl.t)
  method get k = Hashtbl.find hash (String.lowercase k)
  method set k v = Hashtbl.replace hash (String.lowercase k) v
  method each f = Hashtbl.iter f hash
end

let () =
  let tab = new folded_hash 2 in
  tab.{"VILLAIN"} <- "big ";
  tab.{"herOine"} <- "red riding hood";
  tab.{"villain"} <- tab.{"villain"} ^ "bad wolf";
  tab#each (Printf.printf "%s is %s\n")    




(* Hash that permits key *or* value lookups. *)
class ['a] rev_hash size = object
  val hash = (Hashtbl.create size : ('a, 'a) Hashtbl.t)
  method get k = Hashtbl.find hash k
  method set k v =
    Hashtbl.replace hash k v;
    Hashtbl.replace hash v k
  method each f = Hashtbl.iter f hash
end

(*-----------------------------*)

let () =
  let tab = new rev_hash 8 in
  tab.{`Str "Red"} <- `Str "Rojo";
  tab.{`Str "Blue"} <- `Str "Azul";
  tab.{`Str "Green"} <- `Str "Verde";
  tab.{`Str "EVIL"} <- `StrList [ "No way!"; "Way!!" ];
  let to_string = function
    | `Str s -> s
    | `StrList ss -> "[" ^ String.concat " " ss ^ "]" in
  tab#each
    (fun k v ->
       Printf.printf "%s => %s\n" (to_string k) (to_string v))

(* Tee-like class that outputs to multiple channels at once. *)

class tee channels = object
  method print s = List.iter (fun ch -> output_string ch s) channels
end

let () =
  let tee = new tee [stdout; stderr] in
  tee#print "This line goes to both places.\n";
  flush_all ()

let () =
  let tee = new tee
    (stdout ::
       (Array.to_list
          (Array.init 10
             (fun _ ->
                snd (Filename.open_temp_file "teetest." ""))))) in
  tee#print "This lines goes many places.\n";
  flush_all ()    
