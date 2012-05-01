open Netmime
open Netchannels   
let date =
  Netdate.mk_mail_date ~zone:Netdate.localzone (Unix.time())

let mail_header =
  new basic_mime_header [ "MIME-version", "1.0";
                          "Subject", "Sample mail";
                          "From", "bobzhang1988@gmail.com";
                          "To", "hongboz@seas.upenn.edu";
                          "Date", date;
                          "Content-type", "multipart/mixed" ]
let main_text_header =
  new basic_mime_header [ "Content-type", "text/plain;charset=ISO-8859-1";
                          "Content-transfer-encoding", "quoted-printable";
                        ] 
let main_text_body =
  new memory_mime_body "Hello world!\nThis is a sample mail.\n" 
let att_header =
  new basic_mime_header [ "Content-type", "image/jpeg";
                          "Content-transfer-encoding", "base64";
                          "Content-disposition", "inline;description=\"My photo\"";
                        ] 
let att_body =
  new file_mime_body "test_netmime.ml"
let tree =
  (mail_header, `Parts [ (main_text_header, `Body main_text_body);
                         (att_header, `Body att_body) ] )

let n = ref 0    
let ext_storage_style (header:mime_header) =
  let body = new file_mime_body ("attachments") in
  let filename =
    try
      let disp, disp_params = header#content_disposition () in
      let s = Mimestring.param_value (List.assoc "description" disp_params) in
      let () = print_string s in
      s 
    with Not_found -> ("file" ^ string_of_int !n) in
  let () = incr n in
  (body, new Netchannels.output_channel (open_out filename))

let ()  =
  let (chan:io_obj_channel) = new pipe () in begin 
    with_out_obj_channel (chan:>out_obj_channel)
      (fun chan -> write_mime_message chan tree) ;
    with_in_obj_channel chan (fun chan ->
      let nstr = new Netstream.input_stream (chan:>in_obj_channel) in
      let tree = read_mime_message ~storage_style:ext_storage_style nstr  in
      write_mime_message ~wr_body:false (new output_channel stdout) tree);
    Netsendmail.sendmail ~mailer:"/usr/sbin/sendmail" tree
  end


















