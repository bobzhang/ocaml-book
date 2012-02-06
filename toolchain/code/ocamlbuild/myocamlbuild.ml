


      
open Ocamlbuild_plugin;;
open Command;;

let pdflatex = ref (A"pdflatex");;
let ocamlweb = ref (A"ocamlweb");;

dispatch begin function
  | Before_options ->
      Options.ocaml_cflags := ["-w";"A"]
  | After_options ->
      (*c This avoids the creation of symbolic links to the build directory. *)
      Options.make_links := false
  | After_rules ->
      rule "LaTeX to PDF conversion rule"
        ~prod:"%.pdf"
        ~dep:"%.tex"
        begin fun env _build ->
          (*c [env] is a conversion function that substitutes `\%'
            occurrences according to the targets to which the rule
            applies.  [_build] can be called to build new things
            (dynamic dependencies). *)
          let tex = env "%.tex" and _pdf = env "%.pdf" in
          let tags = tags_of_pathname tex++"compile"++"LaTeX"++"pdf" in
          (*c [S]  is for giving a sequence of command pieces.
              [A]  is for atoms.
              [P]  is for pathnames.
              [Px] is a special pathname that should be the main product of the
                   rule (for display purposes).
              [T]  is for tags.
              The other constructors are given in the documentation of the
              [Command] module in [Signatures.COMMAND]. *)
          let cmd = Cmd(S[!pdflatex; T tags; P tex; Sh"< /dev/null"]) in
          (*c Hoping that \LaTeX will converge in two iterations *)
          Seq[cmd; cmd]
        end;
      (*c Here we make an extension of any rule that produces a command
          containing these tags. *)
      flag ["compile"; "LaTeX"; "pdf"; "safe"] (A"-halt-on-error");
      tag_file "master.tex" ["safe"];
      (*c The [dep] function takes tags and pathnames. This will build pathnames
          if a command contains these tags. Note that every file [some_file_name] is
          tagged [file:some_file_name]. *)
      dep ["compile"; "LaTeX"; "pdf"; "file:manual.tex"]
          ["ocamlweb.sty"; "myocamlbuild.tex"];
      rule "OCaml to LaTeX conversion rule (using ocamlweb)"
        ~prod:"%.tex"
        ~dep:"%.ml"
        begin fun env _build ->
          let tex = env "%.tex" and ml = env "%.ml" in
          let tags = tags_of_pathname ml++"ocamlweb"++"LaTeX" in
          Cmd(S[!ocamlweb; T tags; P ml; A"-o"; Px tex])
        end;
end;;

  (* | Before_hygiene -> *)
  (*     (\*c Here you can dynamically tag some files or directories. *\) *)
  (*     (\*c This is done here by checking the [SOME_COND] variable which is *)
  (*         impossible in the \tags file. *\) *)
  (*     if getenv "SOME_COND" ~default:"false" = "true" then *)
  (*       (\*c By setting foo\_dir as not\_hygienic one say that the foo directory *)
  (*           can contains non hygienic files (such as \texttt{.cmi}, \texttt{.cmo}\ldots). *\) *)
  (*       tag_file "foo_dir" ["not_hygienic"] *)
