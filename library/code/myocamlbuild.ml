open Ocamlbuild_plugin
open Command 

let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"
(** ocamlfind can only handle these two flags *)
let find_syntaxes () = ["camlp4o"; "camlp4r"]

let trim_endline str = 
  let len = String.length (str) in 
  if len = 0 then str 
  else if str.[len-1] = '\n' 
  then String.sub str 0 (len-1)
  else str 

(** list extensions, but not used here *)    
let extensions () = 
  let pas = List.filter 
    (fun x ->
      String.contains_string x  0 "pa_" <> None) (find_packages ()) in 
  let tbl = List.map 
    (fun pkg -> 
      let dir = 
        trim_endline (run_and_read ("ocamlfind query " ^ pkg))in 
      (pkg, dir)) pas in 
  tbl

let debug = ref false

let site_lib () =
  trim_endline (run_and_read ("ocamlfind printconf destdir"))

let _ = 
  if !debug then begin 
    List.iter (fun (pkg,dir) -> Printf.printf "%s,%s\n" pkg dir)
      (extensions ()); 
    Printf.printf "%s\n" (site_lib())
  end



(* Menhir options *)
let menhir_opts = S
	[A"--dump";A"--explain"; A"--infer";]

let ocamlfind x = S[A"ocamlfind"; x]
module Default = struct
  let before_options () = 
    Options.ocamlc     := ocamlfind & A"ocamlc";
    Options.ocamlopt   := ocamlfind & A"ocamlopt";
    Options.ocamldep   := ocamlfind & A"ocamldep";
    Options.ocamldoc   := ocamlfind & A"ocamldoc";
    Options.ocamlmktop := ocamlfind & A"ocamlmktop"
  let after_rules () = 
  (*when one link an ocaml library/binary/package, should use -linkpkg*)
    flag ["ocaml"; "byte"; "link";"program"] & A"-linkpkg";
    flag ["ocaml"; "native"; "link";"program"] & A"-linkpkg";
    List.iter begin fun pkg ->
      flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["menhir"] menhir_opts; (* add support for menhir*)
    end (find_packages ());
  (* Like -package but for extensions syntax. Morover -syntax is
   * useless when linking. *)
    List.iter begin fun syntax ->
      flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "infer_interface";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
    end (find_syntaxes ());
  (* The default "thread" tag is not compatible with ocamlfind.
     Indeed, the default rules add the "threads.cma" or
     "threads.cmxa" options when using this tag. When using the
     "-linkpkg" option with ocamlfind, this module will then be
     added twice on the command line.
     To solve this, one approach is to add the "-thread" option when using
     the "threads" package using the previous plugin.
  *)
    flag ["ocaml"; "pkg_threads"; "compile"]  (S[A "-thread"]);
    flag ["ocaml"; "pkg_threads"; "link"]     (S[A "-thread"]);
    flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])
end

module Batteries = struct 
  let before_options () =
    ()
  let after_rules () = 
    let cl_use_batteries = [A"-package"; A"batteries"]
    (* and cl_use_batteries_o = [] *)
    (* and cl_camlp4o = [A"-syntax"; A"camlp4o"] *)
    (* and cl_camlp4r = [A"-syntax"; A"camlp4r"]  *)
    in
    flag ["ocaml"; "compile"; "use_batteries"] & S cl_use_batteries;
    flag ["ocaml"; "ocamldep"; "use_batteries"] & S cl_use_batteries ;
    flag ["ocaml"; "doc";      "use_batteries"] & S cl_use_batteries ;
    flag ["ocaml"; "link";     "use_batteries"] & S cl_use_batteries 

end

module Camlp4 = struct 

  (** ocamlfind ocamlc -package str -c 
     -pp 'camlp4o pa_float.cmo' -o bar.cmo bar.ml 
      pa_comprehension, pp -> -pp '.... '
  *)
  let before_options () = ()
  (**
     I linked pa_ulex.cma to camlp4 directory, to make life easier
     if you have pa_xx.syntax, that will be awesome 
     so in your _tags file 
     <your.ml> : use_ulex
     <your.{byte,native}> : pkg_ulex
  *)
  (** dispatch begin function | After_rules -> flag ["ocaml"; "pp";
      "use_ulex"] (S[A"camlp4o"; A"pa_ulex.cma"]); dep ["ocaml"; "ocamldep";
      "use_ulex"] ["pa_ulex.cma"]; ocaml_lib ~tag_name:"use_ulex" "ulexing";
      | _ -> () end;;
  *)
  let after_rules () = 
    ()
    (* flag ["ocaml"; "pp"; "use_lex"] (S[A"camlp4o"; A"pa_ulex.cma"]); *)
    (* flag ["ocaml";"ocamldep"; "use_ulex";"pp"] (S[A"camlp4o"; A"pa_ulex.cma"]) *)
(*
  let after_rules () = 
  let (@>) = Filename.concat in 

  flag ["comprehension"] &
  (A"'pa_comprehension.cmo'")
(* A( (site_lib () @> "batteries" @> "pa_comprehension.cmo")) *)
    *)
end

let _ = dispatch begin function 
  |Before_options -> 
    Default.before_options ();
    Batteries.before_options ();
    Camlp4.before_options ()
  |After_rules -> 
    Default.after_rules ();
    Batteries.after_rules ();
    Camlp4.after_rules ()
  | _ -> ()
end 

