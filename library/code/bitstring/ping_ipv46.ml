open Printf
open Bitstring

let display pkt = bitmatch pkt with
  | {4:4
    ;hdrlen: 4
    ;tos:8
    ;length:16
    ;identification:16
    ;flags:3
    ;fragoffset:13
    ;ttl:8
    ;protocol:8
    ;checksum:16
    ;source:32
    ;dest:32
    ;options:(hdrlen-5)*32:bitstring;
    payload:-1:bitstring
    } ->    let options = string_of_bitstring  options in
	    let payload = string_of_bitstring  payload in 
	    print_string <:here<
IPV4:
      header length : ${hdrlen,%d}
      type of service : ${tos,%d}
      packet lengh : ${length,%d}
      identification : ${identification,%d}
      flags : ${flags,%d}
      fragment offset : ${fragoffset,%d}
      ttl : ${ttl,%d}
      protocol : ${protocol,%d}
      checksum : ${checksum,%d}
      source : ${source,%ld}
      dest : ${dest,%ld}
      header options + padding : options:$options
      payload:
	$payload
>>
  |{6:4
   ;tclass:8
   ;flow:20
   ;length:16
   ;nexthdr:8
   ;ttl:8
   ;source:128:bitstring
   ;dest:128:bitstring
   ;payload:-1:bitstring} ->
    let source = string_of_bitstring source in
    let dest = string_of_bitstring dest in 
    let payload = string_of_bitstring payload in 
    print_string <:here<
Ipv6:
      traffic class ${tclass,%d}
      flow label: ${flow,%d}
      packet (payload) length: ${length,%d}
      next header : ${nexthdr,%d}
      ttl : ${ttl,%d}
      source address : $source
      destination address : $dest
      payload : $payload
>>

let (|>) x f = f x
let _ =  begin
  bitstring_of_file "ping.ipv4" |> display;
  bitstring_of_file "ping.ipv6" |> display;
end 

