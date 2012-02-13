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


type actions =  (unit -> unit) list ref
    
let before_options : actions = ref []
and after_options : actions = ref []
and before_rules : actions = ref []
and after_rules : actions = ref []

let (+>) x l =
  l := x :: !l
    
let apply  plugin = begin
  Default.before_options +> before_options;
  Default.after_rules +> after_rules;
  (** for pa_ulex, you must create the symbol link by yourself*)
  (fun _ -> flag ["ocaml"; "pp"; "use_ulex"] (A"pa_ulex.cma")) +> after_rules;
  (fun _ -> flag ["ocaml"; "pp"; "use_bolt"] (A"bolt_pp.cmo")) +> after_rules;
  (fun _ ->
    flag ["ocaml"; "pp"; "use_bitstring"]
    (S[A"bitstring.cma"; A"bitstring_persistent.cma"; A"pa_bitstring.cmo"])) +> after_rules;
  (* (fun _ -> *)
  (*   dep ["ocamldep"; "file:test/test_string.ml"] *)
  (*     ["test/test_data/string.txt"; *)
  (*      "test/test_data/char.txt"]) +> after_rules; *)

  (fun _ -> begin 
    flag ["ocaml"; "pp"; "use_python"]
      (S[A"test.cmo"; A"translate.cmo"]);
    flag ["ocaml"; "pp"; "use_list"]
      (S[A"pa_list.cmo"]);
  end ) +> after_rules;
  (fun _ -> begin
    dep ["ocamldep";  "use_python"] ["test.cmo"; "translate.cmo"];
    dep ["ocamldep";  "use_list"] ["pa_list.cmo"];
    Options.ocaml_lflags := "-linkall" :: !Options.ocaml_lflags;
  end 
    ) +> after_rules;

  
  (* (fun _ -> *)
  (*   dep ["file:test/test_string.byte"] ["test/test_data/string.txt"]) +> after_rules; *)
  plugin ();
  dispatch begin function
    | Before_options -> begin
      List.iter (fun f -> f () ) !before_options;
    end
    | After_rules -> begin
      List.iter (fun f -> f ()) !after_rules;
    end
    | _ -> ()
  end ;
end 


let _ =
  (** customize your plugin here *)
  let plugin = (fun _ -> ())  in 
  apply plugin
 


(**
   customized local filter 
*)    
(* let _ = dispatch begin function *)
(*   |After_rules -> begin  *)
(*     flag ["ocaml"; "pp"; "use_filter"] (A"pa_filter.cma"); *)
(*     dep ["ocaml"; "ocamldep"; "use_filter"] ["pa_filter.cma"]; *)
(*   end  *)
(*   |_ -> () *)
(* end *) 
