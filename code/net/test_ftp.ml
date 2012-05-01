open Format

#require "netclient";;

open Ftp_client


let buffer = Buffer.create 1000 
let ch = new Netchannels.output_buffer buffer
let client = new ftp_client ()

let () = begin
  client#exec (connect_method ~host:"127.0.0.1" ());
  client#exec (login_method ~user:"foo"
	      ~get_password:(fun () -> "password")
	      ~get_account:(fun () -> "foo") ());
  client#exec (get_method
	      ~file:(`NVFS "/home1/h/hongboz/SourceCode/godi-rocketboost-20110811/ocaml/build/distfiles/ocaml-3.12.1/bytecomp/lambda.ml") ~representation:`Image
	      ~store:(fun _ -> `File_structure ch)	 
		 ())
end


















