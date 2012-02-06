

open Netchannels

let sum_up (ch:in_obj_channel) =
  let sum = ref 0 in
  try
    while true do
      let line = ch#input_line() in
      sum := !sum + int_of_string line
    done;
    assert false;
  with
      End_of_file -> !sum


let html_document file =
  with_in_obj_channel
    (new input_channel (Pervasives.open_in file))
    Nethtml.parse

let write  file html_document = 
  with_out_obj_channel
    (new output_channel (Pervasives.open_out file))
    (fun ch -> Nethtml.write ch html_document)
