


module type S = sig
  type 'a perms
  type 'a t
  val read_only : [`Readable] perms
  val write_only : [`Writable] perms
  val read_write : [> `Readable | `Writable] perms
  (** interesting 'a perms -> 'a t, both perms and t are phantom
      types *)
  val map_file : string -> 'a perms -> int -> 'a t
  val get : [>`Readable] t -> int -> char
  val set : [>`Writable] t -> int -> char -> unit
end

(**
   Array1.map_file;;
   - : Unix.file_descr ->
   ?pos:int64 ->
   ('a, 'b) Batteries.Bigarray.kind ->
   'c Batteries.Bigarray.layout ->
   bool -> int -> ('a, 'b, 'c) Batteries.Bigarray.Array1.t

   Array1.get;;
   - : ('a, 'b, 'c) Batteries.Bigarray.Array1.t -> int -> 'a = <fun>p

   Array1.set;;
   - : ('a, 'b, 'c) Batteries.Bigarray.Array1.t -> int -> 'a -> unit = <fun>
   
*)  
module M : S= struct
  open Unix
  open Bigarray
  type bytes = (int,int8_unsigned_elt,c_layout) Bigarray.Array1.t
  type 'a perms = int
  type 'a t = bytes
  let read_only = 1
  let write_only = 2
  let read_write = 3
  let openflags_of_perms n = match n with
    | 1 -> O_RDONLY, 0o400
    | 2 -> O_WRONLY, 0o200
    | 3 -> O_RDWR, 0o600
    | _ -> invalid_arg "access_of_openflags"
  let access_of_openflags  = function
    | O_RDONLY -> [R_OK;F_OK]
    | O_WRONLY -> [W_OK; F_OK]
    | O_RDWR -> [R_OK; W_OK;F_OK]
    | _ -> invalid_arg "access_of_openflags"
      
  let map_file filename perms sz =
    let oflags,fperm = openflags_of_perms perms in
    try
      access filename (access_of_openflags oflags);
      let fd = openfile filename [oflags;O_CREAT] fperm in
      Array1.map_file fd int8_unsigned c_layout false sz
    with
        _ ->
          invalid_arg "map_file: not even a valid permission"

  let  get a i = Char.chr (Array1.get a i)
  let set a i c = Array1.set a i (Char.code c)

end 
