open Facile
open Easy

(** THE All Different constraint **)
let cards = Array.init 9 (fun i -> Fd.int 1, (i+1))
let ad = fun a -> Cstr.post (Gcc.cstr ~level:Gcc.High a cards)

let solve grille =
   (** The matrix of variables **)
   let v = Array.init 9 (fun _ -> Fd.array 9 1 9) in

   (** Preset values **)
   Array.iteri
     (fun i vi ->
       Array.iteri
     (fun j vij ->
       let c = grille i j in
       if c <> '.' then
             Fd.unify vij (Char.code c - Char.code '0'))
         vi)
     v;

   (** Lines **)
   Array.iter ad v;

   (** Columns **)
   for i = 0 to 8 do
     ad (Array.init 9 (fun j -> v.(j).(i)))
   done;

   (** Regions **)
   for i = 0 to 2 do
     for j = 0 to 2 do
       ad (Array.init 9 (fun k -> v.(3*i+k/3).(3*j+k mod 3)))
     done
   done;

   (** Print **)
   Array.iter (fun vi -> Fd.fprint_array stdout vi; print_newline ()) v;
   print_newline ()


(** Solving all the grids from a file (one per line) **)
let _ =
   let f = open_in Sys.argv.(1) in
   try
     while true do
       let l = input_line f in
       solve (fun i j -> l.[9*i+j])
     done
   with
     End_of_file -> ()
