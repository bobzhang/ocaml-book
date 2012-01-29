
open Camlp4.PreCast
  
let  ( *** ) f g (a,b) = (f a , g b)

let get_loc_err (loc:Loc.t) (msg:string) : string  = Loc.(
  sprintf "File \"%s\", line %d, characters %d-%d: %s"
    (file_name loc) (start_line loc)
    (start_off loc - start_bol loc)
    (stop_off loc - stop_bol loc)
    msg
)

let hash_variant (str:string) :int =
  let acc_ref = ref 0 in
  for i = 0 to String.length str - 1 do
    acc_ref := 233 * !acc_ref + Char.code str.[1]
  done ;
  if Sys.word_size = 32 then !acc_ref
  else !acc_ref land int_of_string "0x7FFFFFFF"

type pat =
  | Not_initialized
  | Too_late
  | Path of string * (string list) (** Hierachical structure *)

let con_path_ref = ref Not_initialized      

let get_conv_path_el () : (string * string list) =
  match !conv_path_ref with
  | Path (e, el) -> e, el
  | _ -> failwith "Pa_type_conv: path not set"


(** Get path to the currently preprocessed module *)
let get_conv_path () = fst (get_conv_path_el ())

(** Set path to the currently preprocessed module *)
let set_conv_path conv_path =
  if !conv_path_ref = Not_initialized || !Sys.interactive then
    (** interesting*)
    conv_path_ref := Path (conv_path, [conv_path])
  else failwith "Pa_type_conv: module name set twice"

let _ = if !Sys.interactive then set_conv_path "Toplevel"

let push_conv_path mod_name =
  match !conv_path_ref with
  | Not_initialized -> conv_path_ref := Too_late (* Entered a submodule *)
  | Too_late -> ()
  | Path (str, rev_lst) ->
      conv_path_ref := Path (str ^ "." ^ mod_name, mod_name :: rev_lst)

let pop_conv_path _ =
  match !conv_path_ref with
  | Path (_, _ :: rev_lst) ->
      conv_path_ref := Path (String.concat "." (List.rev rev_lst), rev_lst)
  | _ -> ()



let generators, sig_generators,  exn_generators, sig_exn_generators
  = Hashtbl.create 0, Hashtbl.create 0, Hashtbl.create 0, Hashtbl.create 0


(** Check that there is no argument for generators that do not expect
    arguments *)
let no_arg id e typ arg =
  if arg = None then e typ
  else
    failwith (
      "Pa_type_conv: generator '" ^ id ^ "' does not expect an argument")

(* Parse a list of tokens with the given grammar entry *)
let parse_with entry = function
  | Some tokens ->
      Some (Gram.parse_tokens_after_filter entry (Stream.of_list tokens))
  | None -> None

(* Entry which ignores its input *)
let ignore_tokens = Gram.Entry.of_parser "ignore_tokens" ignore

(* Add new generator, fail if already defined *)
let safe_add_gen gens id entry e =
  if Hashtbl.mem gens id then
    failwith ("Pa_type_conv: generator '" ^ id ^ "' defined multiple times")
  else Hashtbl.add gens id (fun typ arg -> e typ (parse_with entry arg))

(* Register a "with"-generator for types in structures *)
let add_generator_with_arg ?(is_exn = false) id entry e =
  let gens = if is_exn then exn_generators else generators in
  safe_add_gen gens id entry e

let add_generator ?is_exn id e =
  add_generator_with_arg ?is_exn id ignore_tokens (no_arg id e)

(* Removes a "with"-generator for types in structures *)
let rm_generator ?(is_exn = false) id =
  let gens = if is_exn then exn_generators else generators in
  Hashtbl.remove gens id

(* Register a "with"-generator for types in signatures *)
let add_sig_generator_with_arg ?(is_exn = false) id entry e =
  let gens = if is_exn then sig_exn_generators else sig_generators in
  safe_add_gen gens id entry e

let add_sig_generator ?is_exn id e =
  add_sig_generator_with_arg ?is_exn id ignore_tokens (no_arg id e)

(* Removes a "with"-generator for types in signatures *)
let rm_sig_generator ?(is_exn = false) id =
  let gens = if is_exn then sig_exn_generators else sig_generators in
  Hashtbl.remove gens id




