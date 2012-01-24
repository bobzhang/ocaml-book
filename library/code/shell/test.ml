#require "shcaml";;

let processes =
  LineShtream.string_list_of ^$
    run_source
    (ps () -| cut Line.Ps.command )

(** hello : Line.empty Line.t *)
let hello =
  Line.line "hello world, I'm a line"
(** hello_delim : < delim : < .. >; ..  > Line.t *)
let hello_delim =
  Line.Delim.create
    (Pcre.asplit ~pat:", " (Line.show hello))
    hello

(** Line.Delim.fields;;
- : < delim : < .. >; .. > Shcaml.Line.t -> string array = <fun> *)    
let bs = Line.Delim.fields hello_delim

let hello_DELIM =
  Line.Delim.set_fields
    (Array.map String.uppercase (Line.Delim.fields hello_dim))
    hello_delim
  
let uppercase_delims ln =
  Line.Delim.set_fields
    (Array.map String.uppercase (Line.Delim.fields ln))
    ln 

let root = Line.line "root:x:0:0:Enoch Root:/root:/bin/shcaml"

let root_delim = Line.Delim.create
  (Pcre.asplit ~pat:":" (Line.show root)) root
  
let passwd_of_delim ln =
  match Line.Delim.fields ln with
    | [|name;passwd;uid;gid;gecos;home;shell|] ->
      Line.Passwd.create
	~name ~passwd ~gecos ~home ~shell
	~uid:(int_of_string uid)
	~gid:(int_of_string gid)
	ln
    | _ -> Shtream.warn "Line didn't have 7 fields"

let root_pwd = passwd_of_delim root_delim
let uid = Line.Passwd.uid root_pwd  

let root_un = Line.select Line.Passwd.name root_pwd

let stdin_shtream = Shtream.of_channel Legacy.input_line Legacy.stdin   
(* Shtream.next stdin_shtream;; *)
let newstdin = Shtream.channel_of print_endline stdin_shtream  

let _ =
  "echo a fitting" |> command |> run

(** command "foo bar baz"
    is like sh -c "foo bar baz"
*)

let goodbye = "echo goodbye from unix" |> command in begin
  print_endline "hello from ocaml";
  goodbye |> run 
end 

let _ = begin
  "cat /etc/passwd" |> command |> run ;
  "/etc/passwd" |> from_file |> run ;
  `Filename "/etc/passwd" |> from_gen |> run 
end 

let passwd = "/etc/passwd" |> from_file |> run_source  


let pw_shtream = run_source
    (from_file "/etc/passwd" -| Adaptor.Passwd.fitting ())

let shells = LineShtream.string_list_of
  (run_source
  (from_file "/etc/passwd"
  -| Adaptor.Passwd.fitting ()
  -| cut Line.Passwd.shell
  -| command "sort"
  -| uniq ()))


let a =
  run (command "echo hello" />/ [Legacy.stdout />* `Null ])

let b =
  run (command "echo hello" />/ [1 %>* `Filename "/dev/null"])


    
let _ = run begin
  from_file "/etc/passwd"   -|
  Adaptor.Passwd.fitting () -|
  grep (fun line -> Passwd.shell line = "/usr/bin/schs") -|
  cut Passwd.name -|
  mail_to ~subject:"A New Shell" ~msg:"Check out Caml-Shcaml!"
end 
