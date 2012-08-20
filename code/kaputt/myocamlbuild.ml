open Ocamlbuild_plugin
open Ocamlbuild_pack  
open Command
open Printf 
open Tags.Operators
open Tags

  
(** interactive with toplevel
    #directory "+ocamlbuild";;
    #load "ocamlbuildlib.cma";;
   for interactive debugging
 *)

(** utility functions *)
let doc_modules = ref StringSet.empty;;
let verbose = ref false
let debug = ref false    
let lib_files : (string, string list)Hashtbl.t = Hashtbl.create 50
let version = "1.0"
let time = Unix.(
  let {tm_mon;tm_mday;tm_year;
       tm_hour;tm_min;
       tm_sec} =gmtime (time ()) in
  sprintf "%02d-%02d-%04d %02d:%02d:%02d UTC"
    (tm_mon+1) tm_mday (tm_year + 1900)
    tm_hour tm_min tm_sec
 )

let flip f x y = f y x
let trim_endline str = 
  let len = String.length (str) in 
  if len = 0 then str 
  else if str.[len-1] = '\n' 
  then String.sub str 0 (len-1)
  else str
      
(** mainly in place of _tags file *)          
let merge_files files =
  String.concat "or"
    (List.map (fun f -> "<" ^ f ^ ">") files)
let merge_tags tags = String.concat "," tags 
let input s = Configuration.parse_string s   
let prerr_endlinef fmt =
    ksprintf (fun str-> if !verbose then prerr_endline str) fmt
let run_and_read      = Ocamlbuild_pack.My_unix.run_and_read
let blank_sep_strings = Ocamlbuild_pack.Lexers.blank_sep_strings
let opt_bind x f = match x with
 |Some v -> f v
 |None -> None

type file_type = | Inferred | Ml | Mli | Pp_ml | Ppo_ml | Cmo | Cma | Cmi | Cmx
  | Cmxa | Cmxs  | Mllib    | Mldylib  | Odocl
let string_of_file_type = function
  | Inferred -> ".inferred.mli"
  | Mli -> ".mli"
  | Ml -> ".ml"
  | Pp_ml -> "_pp.ml"
  | Ppo_ml -> "_ppo.ml"
  | Cmo -> ".cmo"
  | Cma -> ".cma"
  | Cmi -> ".cmi"
  | Cmx-> ".cmx"
  | Cmxa -> ".cmxa"
  | Cmxs -> ".cmxs"
  | Mllib -> ".mllib"
  | Mldylib -> ".mldylib"
  | Odocl -> ".odocl"
module Opt = struct     
  let (|*>) = tag_file
  let (|**>) files tag = List.iter (fun f -> f |*> tag) files
  let (//) = Filename.concat      
  let (/*>) base ty =
    base ^ (string_of_file_type ty)
  let (|-?) fileA files = dep ["ocamldep"; "file:"^fileA] files 
  let (|-??) fileAs files = List.iter (fun fileA -> fileA |-? files) fileAs
  let (<+>) files tags = begin 
    let src = (merge_files files ^ ":" ^ merge_tags tags) in
    Log.dprintf 2 "tags: %s\n" src;
    input src
  end 
end;;

open Opt           
    
(** Functions defined to drive rules*)
(*stolen from Ocaml_specific.ml*)
module Driver = struct

  let default_camlp4r () =
     ["*.ml"] <+> ["camlp4rf.opt"; "use_camlp4"]

  (* FIXME what will happen when
     the tag is a/b? *)        
  let mk_local t tag =
    let name = tag /*> t in  
    let use_tag = "use_"^tag in  begin 
      flag ["ocaml";"pp"; use_tag] (A name);
      dep ["ocamldep"; use_tag] [name];
      Log.dprintf 2  "create tag :%s" use_tag;
      use_tag
    end
  let camlp4_flags camlp4s =
    List.iter begin fun camlp4 ->
      flag ["ocaml"; "pp"; camlp4] (A camlp4)
    end camlp4s
  let camlp4_flags' camlp4s =
    List.iter begin fun (camlp4, flags) ->
      flag ["ocaml"; "pp"; camlp4] flags
    end camlp4s


  let camlp4 ?(default = A "camlp4r.opt") ?(printer=A "r")
      tag i o env build = (
    let ml = env i and pp_ml = env o in
    (**  add a pp here to triger the rule pp,
         camlp4rf ==> camlp4rf  don't tag file pp,camlp4rf, it will be inherited by
         .cmo file, and cause trouble there  *)
    let tags = (((tags_of_pathname ml) ++ "ocaml" ++ "pp") ) ++ tag in
    (*
     * add a ocamldep here to trigger the rule
     *     ocamldep, use_geneq => examples/geneq.cma
     *     Rule.build_deps_of_tags will try to build the deps  *)
    let _deps = Rule.build_deps_of_tags build (tags ++ "ocamldep") in
    let pp = Command.reduce (Flags.of_tags tags) in
    let pp = match pp with | N -> default | _ -> pp in
    Cmd (S [ pp; P ml; A "-printer";printer; A "-o"; Px pp_ml ])
   )
  let infer_with_error_channel ?(ocamlc=Options.ocamlc) flag tag =
    let infer ml dlambda env build = let open Ocaml_utils in
    let ml = env ml and dlambda = env dlambda in
    let tags = tags_of_pathname ml ++ "ocaml" in
    Ocaml_compiler.prepare_compile build ml ;
    Cmd(S[!ocamlc; ocaml_ppflags tags; ocaml_include_flags ml;
          A flag;
          (if Tags.mem "thread" tags then A"-thread" else N);
          T(tags++tag); P ml; Sh"2>"; Px dlambda]) in
    infer 
  let infer_dlambda =  infer_with_error_channel "-dlambda" "infer_dlambda"
  let infer_drawlambda = infer_with_error_channel "-drawlambda" "infer_drawlambda"
  let infer_dparsetree = infer_with_error_channel "-dparsetree" "infer_dparsetree"
  let infer_instr =  infer_with_error_channel "-dinstr" "infer_instr"
  let infer_dclambda =  infer_with_error_channel
      ~ocamlc:Options.ocamlopt "-dclambda" "infer_dclambda"
  let infer_dcmm = infer_with_error_channel
      ~ocamlc:Options.ocamlopt "-dcmm" "infer_dcmm" 
  let infer_dlinear = infer_with_error_channel
      ~ocamlc:Options.ocamlopt "-dlinear" "infer_dlinear";;
  let mk_odocl _ _ =
    let modules = String.concat "\n" (StringSet.elements !doc_modules) in
    Cmd (S[A"echo"; Quote(Sh modules); Sh">"; P ("foo" /*> Odocl)])
      
  let mk_lib  suffix env build =
    let m = env "%" in
    try
      let lst = String.concat "\n" & Hashtbl.find lib_files m in
      (* if not (Sys.file_exists (m^suffix)) then  *)
      Cmd (S[A"echo"; Quote(Sh lst); Sh ">"; P (m /*> suffix )])
      (* else Nop *)
    with Not_found -> begin
      Log.dprintf 2
        "Warning: %s not defined in lib table, using default linking rule" m;
      raise Rule.Failed
    end
  let mk_mllib = mk_lib Mllib
  let mk_mldylib = mk_lib Mldylib
  let mk_version _ _ = (
    let cmd =
      sprintf "let version = %S\n\
let compile_time = %S" 
    version time in
    Cmd (S[A"echo"; Quote (Sh cmd); Sh ">"; P"version.ml"]))

  let myocamldoc tags =
    Ocaml_tools.ocamldoc_l_dir (tags -- "extension:html") 

end;;
    
open Driver;;
(** My rules *)
 begin (
   camlp4_flags ["camlp4o.opt"; "camlp4r.opt";
                 "camlp4of.opt"; "camlp4rf.opt";
                 "camlp4orf.opt"; "camlp4oof.opt"];
   camlp4_flags' ["camlp4orr.opt", S[A"camlp4of.opt"; A"-parser"; A"reloaded"];
                  "camlp4rrr.opt", S[A"camlp4rf.opt"; A"-parser"; A"reloaded"]];

   rule "ocaml: ml & ml.depends  -> .dlambda" ~prod:"%.dlambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_dlambda "%.ml" "%.dlambda");
   rule "ocaml: ml & ml.depends  -> .drawlambda"
     ~prod:"%.drawlambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_drawlambda "%.ml" "%.drawlambda");
   rule "ocaml: ml  -> .dparsetree"
     ~prod:"%.dparsetree" ~deps:["%.ml"]
     (infer_dparsetree "%.ml" "%.dparsetree");
   rule "ocaml: ml & ml.depends  -> .dinstr"
     ~prod:"%.dinstr" ~deps:["%.ml";"%.ml.depends"]
     (infer_instr "%.ml" "%.dinstr");
   rule "ocaml: ml & ml.depends  -> .dclambda"
     ~prod:"%.dclambda" ~deps:["%.ml";"%.ml.depends"]
     (infer_dclambda "%.ml" "%.dclambda");
   rule "ocaml: ml & ml.depends  -> .dcmm"
     ~prod:"%.dcmm" ~deps:["%.ml";"%.ml.depends"]
     (infer_dcmm "%.ml" "%.dcmm");
   rule "ocaml: ml & ml.depends  -> .dlinear"
     ~prod:"%.dlinear" ~deps:["%.ml";"%.ml.depends"]
     (infer_dlinear "%.ml" "%.dlinear");
   rule "ocaml: mldylib & cmx* & o* -> cmxs"
     ~tags:["ocaml"; "native"; "shared"; "library"]
     ~prods:["%.cmxs"]
     ~dep:"%.mldylib"
     (Ocaml_compiler.native_shared_library_link_mldylib "%.mldylib" "%.cmxs");
   rule "foo.odocl" ~prod:"foo.odocl" mk_odocl;
   rule "generate %.mllib" ~prod:"%.mllib" mk_mllib;
   rule "generate %.mldylib" ~prod:"%.mldylib" mk_mldylib;
   rule "version.ml" ~prod:"version.ml" mk_version;
   rule "preprocess: ml -> _ppr.ml" ~dep: "%.ml" ~prod:"%_ppr.ml"
    (camlp4 "%_ppr.ml" "%.ml" "%_ppr.ml");
   rule "preprocess: ml -> _ppo.ml" ~dep: "%.ml" ~prod: "%_ppo.ml"
    (camlp4 ~printer:(A"o") "%_ppo.ml" "%.ml" "%_ppo.ml");
   rule "ocamldoc: use plugin"
     ~dep:"%.odocl" ~stamp:"%.docdir/html.stamp" ~prod:"%.docdir/index.html"
     ~insert:`top
     (Ocaml_tools.document_ocaml_project ~ocamldoc:myocamldoc
       "%.odocl" "%.docdir/index.html" "%.docdir");
  )
 end 

let file_deps   (bs:string list) (tybs: file_type list)  (deps:string list)
    (tydeps:file_type list) = let open List in 
  let files = concat & map (fun f ->
    (map (fun ty -> f /*> ty) tydeps)) deps in
  let bases = concat & map (fun f ->
     (map (fun ty -> f /*> ty) tybs)) bs in
  bases |-?? files
let deps_mli f files = file_deps [f]  [Ml;Pp_ml;Ppo_ml] files [Inferred];;    
let deps_mli_table (tbl: (string* string list ) list) =
  List.iter (fun (s,lst) ->
    deps_mli s lst ) tbl


(**
   configuration syntax extensions
   the key is used by ocamlfind query to get its path.
   for example: ocamlfind query bitstring
*)    
let syntax_lib_file
    = ["bitstring",[`D "bitstring.cma" ;
		    `D "bitstring_persistent.cma";
		    `D "pa_bitstring.cmo"]
      ;"ulex",     [`D "pa_ulex.cma"]
      ;"bolt",     [`D "bolt_pp.cmo"]
      ;"xstrp4",   [`D "xstrp4.cma"]
      ;"sexplib",     [`P ("type-conv", "Pa_type_conv.cma"); `D "pa_sexp_conv.cma"]
      ;"mikmatch_pcre", [`D "pa_mikmatch_pcre.cma"]
      ;"meta_filter",    [`D "meta_filter.cma"]
      ;"text", [`D "text.cma"; `D "text-pcre-syntax.cma"]
      ;"type_conv", [`D "pa_type_conv.cma"]   
      ]
let syntax_lib_file_cache
    = ".syntax_lib_file_cache"
      

let menhir_opts = S [A"--dump";A"--explain"; A"--infer";]

let site_lib () =
  trim_endline (run_and_read ("ocamlfind printconf destdir"))

let argot_installed  () =
  try
    let path = (trim_endline & run_and_read "ocamlfind query argot") in 
    if Sys.(file_exists path) then  begin 
      flag ["ocaml"; "doc"]
        (S[A"-i";
           A path;
           A"-g";
           A"argot.cmo";
           (* A"-search"; *)
         ]);
      Log.dprintf 2 "argot plugin hooked to ocamldoc"
    end 
    else Log.dprintf 2 "argot not installed"
  with
    e -> Log.dprintf 2 "argot not installed"

(** handle package *)    
let find_packages () =
  blank_sep_strings &
    Lexing.from_string &
    run_and_read "ocamlfind list | cut -d' ' -f1"      

(** list extensions for debug purpose *)
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
(** not turned on by default *)    
let _ = 
  if !debug then begin 
    List.iter (fun (pkg,dir) -> Printf.printf "%s,%s\n" pkg dir)
      (extensions ()); 
    Printf.printf "%s\n" (site_lib())
  end


exception Next
let syntax_path syntax_lib_file = (
  if Sys.file_exists syntax_lib_file_cache then begin
    Log.dprintf 2 "read from .syntax_lib_file_cache";
    let chin = open_in syntax_lib_file_cache in 
    let lst = Marshal.from_channel chin in
    (* List.iter (fun (package,(x,y)) -> (flag x y )) lst ; *)
    List.iter (fun (x,_) ->
      try
        let (a,b) = List.assoc x lst in
        flag a b 
      with
        Not_found ->
          Log.dprintf 2 "syntax package %s not setup" x
              ) syntax_lib_file;
    close_in chin ;
  end 
  else begin
    Log.dprintf 2  ".syntax_lib_file_cache not found";
    let chan = open_out syntax_lib_file_cache in
    let args = ref [] in 
    flip List.iter syntax_lib_file (fun (package, files) ->
      try
        (let package_path =
	  try
	    trim_endline & run_and_read ("ocamlfind query " ^ package )
	  with Failure _ ->
	    prerr_endlinef "package %s does not exist" package;
	    raise Next 
        in
        if Sys.file_exists package_path then
	  let all_path_files  =
	    List.map (fun file ->
	      match file with
	      | `D file ->
		  if Sys.file_exists (package_path//file)
		  then (package_path // file)
		  else
		    (prerr_endlinef "%s does not exist " (package_path//file);
		     raise Next)
	      | `P (package,file) ->
		  let sub_pack =
		    try
		      trim_endline & run_and_read ("ocamlfind query " ^ package)
		    with Failure _ -> begin 
		      prerr_endlinef "%s does not exist in subpackage definition" package;
		      raise Next
		    end 
		  in
		  if Sys.file_exists (sub_pack//file) then
		    (sub_pack // file)
		  else
		    (prerr_endlinef "%s does not exist " (sub_pack//file);
		     raise Next )
	             ) files
	  in begin
            args :=
              (package,
               (["ocaml"; "pp"; "use_"^ package],
               (S(List.map (fun file -> A file)
		   all_path_files)))) ::!args
          end 
        else begin 
	  prerr_endlinef "package %s does not exist" package;
        end 
        )
      with Next -> ());
    Marshal.to_channel chan !args [];
    List.iter (fun (package, (x,y)) -> flag x y ) !args;
    close_out chan
  end )
let find_syntaxes () = ["camlp4o"; "camlp4r"]
let ocamlfind x = S[A"ocamlfind"; x]
module Default = struct
  let before_options () = (
    Options.ocamlc     := ocamlfind & S[A"ocamlc"; A"-annot";
                                        A"-warn-error";
                                        A"A"
                                          (* 4-6-7-9-27..29 *)
                                      ];
    Options.ocamlopt   := ocamlfind & S[A"ocamlopt";A"-annot"];
    Options.ocamldep   := ocamlfind & A"ocamldep";
    Options.ocamldoc   := ocamlfind & A"ocamldoc";
    (* Options.ocamldoc := S [A "ocamldoc"]; *)
    (** ocamlfind does not accept -search
        ocamldoc.opt does not work on mac
     *)
    Options.ocamlmktop := ocamlfind & A"ocamlmktop")
  let after_rules () = (
    (*when one link an ocaml library/binary/package, should use -linkpkg*)
    flag ["ocaml"; "byte"; "link";"program"] & A"-linkpkg";
    flag ["ocaml"; "native"; "link";"program"] & A"-linkpkg";
    List.iter ( fun pkg ->
      flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
      flag ["menhir"] menhir_opts; (* add support for menhir*)
    ) (find_packages ());
    (* Like -package but for extensions syntax. Morover -syntax is
     * useless when linking. *)
    List.iter ( fun syntax ->
      flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
      flag ["ocaml"; "infer_interface";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
    ) (find_syntaxes ());
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
    flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"]);
    

    flag["pp"  ; "ocaml"; "use_macro"]  (S[A"-parser"; A"macro"]);
    flag["pp"  ; "ocaml"; "use_map"] (S[A"-filter"; A"map"]);
    flag["pp"  ; "ocaml"; "use_lift"] (S[A"-filter"; A"lift"]);
    flag["pp"  ; "ocaml"; "use_fold"] (S[A"-filter"; A"fold"]);
    flag["pp"  ; "ocaml"; "use_debug"] (S[A"-parser"; A"Camlp4DebugParser.cmo"]);
    flag ["link";"ocaml";"g++";] (S[A"-cc"; A"g++"]);
    flag ["ocaml"; "doc"]  (S [A"-keep-code"]);
    argot_installed ();
    flag ["ocaml"; "doc"; "use_camlp4"] (S[A"-I"; A"+camlp4"]);
   )
end 


type actions =  (unit -> unit) list ref
let before_options : actions = ref []
and after_options : actions = ref []
and before_rules : actions = ref []
and after_rules : actions = ref []
let (+>) x l =  l := x :: !l

(** demo how to use external libraries
    ocaml_lib ~extern:true "llvm";
    ocaml_lib ~extern:true "llvm_analysis";
    ocaml_lib ~extern:true "llvm_bitwriter";
    dep ["link"; "ocaml"; "use_plus_stubs"] ["plus_stubs.o"];
    flag["link"; "ocaml"; "byte"] (S[A"-custom"]);
    dep ["ocamldep"; "file:test_lift_filter_r.ml"] ["test_type_r.ml"];
    dep ["ocamldep"; "file:test_lift_filter.ml"] ["test_type.ml"];
    dep ["ocamldep"; "file:test_dump.ml"] ["test_type_r.ml"];
    dep ["ocamldep"; "file:test_lift_filter.pp.ml"] ["test_type.ml"];
    demo how to use dep
        dep ["ocamldep"; "file:test/test_string.ml"]
        ["test/test_data/string.txt";
        "test/test_data/char.txt"];
    *)


let tags_table : ((string list * string list) list) ref =
  ref [];;

(** Insert most your code here *)                           
let before_options_dispatch () = begin 
  List.iter (fun (xs,ys) -> xs <+> ys ) !tags_table
end 
let apply  () = (
  Default.before_options +> before_options;
  Default.after_rules +> after_rules;
  before_options_dispatch +> before_options;

  dispatch begin function
    | Before_options -> begin
        List.iter (fun f -> f () ) !before_options;
    end
    | After_rules -> begin
        List.iter (fun f -> f ()) !after_rules;
    end
    | _ -> ()
  end ;
 )


(***)    
let _ = begin 
  apply ();
end


