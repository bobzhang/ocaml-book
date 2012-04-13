(**
   integers, strings, sub-bitstrings
   big-, little- and native endianness
   signed unsigned types
   variable-width fields
   fields with arbitrary bit aligment
*)
open Printf
  
let bits = Bitstring.bitstring_of_file "banana.gif" in
bitmatch bits with
  | {("GIF87a"|"GIF89a") : 6 * 8 : string; (* GIF magic *)
     width : 16 : littleendian;
     height : 16 : littleendian
    } ->
    printf "GIF image is %d x %d pixels"
      width height
  | {_} ->
    eprintf "Not a GIF image\n"
    
     
let bits = Bitstring.bitstring_of_file "ls" in
bitmatch bits with
  |{ 0x7f : 8
   ; "ELF" : 24 : string  (* ELF magic number *)
   ; _ : 12*8 : bitstring (* ELF identifier *)
   ; e_type: 16 : littleendian (* object file type *)
   ;  e_machine: 16 : littleendian 
   } ->
    printf "This is an ELF binary, type %d, arch %d\n"
      e_type e_machine

let display pkt =
  bitmatch pkt with
    | { 4:4 (**IPV4*)
      ; hdrlen:4 (* IHL *)
      ; tos:8    (* Type of service *)
      ; length:16 (* TOTAL length *)
      ; identification:16 
      ; flags:3       
      ; fragoffset:13 
      ; ttl:8
      ; protocol:8
      ; checksum:16
      ; source : 32 
      ; dest:32
      ; _ : (hdrlen-5)*32 : bitstring
      ; _ : -1 : bitstring
      } ->
      print_string <:here<
IPv4:
hdrlen: ${hdrlen,%d} tos: ${tos,%d} length: ${length,%d} flags: ${flags,%d}
fragoffset: ${fragoffset,%d} ttl: ${ttl,%d}
protocol: ${protocol,%d} checksum: ${checksum,%d}
identification: ${identification,%d} ${source,%ld} --> ${dest,%ld}
>>
    | {_} ->
      print_string interpolate
	"not known yet \n"

let _ =
  let bits = Bitstring.bitstring_of_file "ping.ipv4" in
  display bits
	
let make_message typ subtype param : Bitstring.bitstring =
  (BITSTRING {
     typ: 16
    ;subtype:16
    ;param:32
  })

(** Internally a bitstring is stored as a normal OCaml string together
    with an offset and length, where the offset and length are measured
    in bits. Thus one can efficiently form substrings of bitstrings,
    overlay a bitstring on existing data, and load and save bitstrings
    from files or other external sources.

    To load a bitstring from a file use Bitstring.bitstring_of_file or
    Bitstring.bitstring_of_chan.
*)    

(** Patterns look a bit different from normal match patterns. They
    consist of a list of bitfields separated by ; where each bitfield
    contains a bind variable, the width (in bits) of the field, and
    other information.
*)

(** You can also add conditional when-clauses:

    | { version : 4 }
    when version = 4 || version = 6 -> ...

(* Only match and run the code when version is 4 or 6.  If
    it isn't we will drop through to the next case. *)
*)



    
(** In terms of regular expressions you might say that the pattern
    matches ^pattern, not ^pattern$. To ensure that the bitstring
    contains only the pattern, add a length -1 bitstring to the end and
    test that its length is zero in the when-clause:
    | { n : 4;
    rest : -1 : bitstring }
    when Bitstring.bitstring_length rest = 0 -> ...

   (* Only matches exactly 4 bits. *)

    Normally the first part of each field is a binding variable, but
    you can also match a constant, as in:

    | { (4|6) : 4 } -> ...

(* Only matches if the first 4 bits contain either the integer 4 or
    the integer 6. *) One may also match on strings:

    | { "MAGIC" : 5*8 : string } -> ...

(* Only matches if the string "MAGIC" appears at the start of the
    input. *)
*)


(**
   bitmatch bits with
   | { field1 : 8;
   field2 : 8 : offset(160) } -> ...

   matches field1 at the start of the bitstring and field2 at 160 bits
   into the bitstring. The middle 152 bits go unmatched (ie. can be
   anything)

*)

(** You can add a check(expr) qualifier to bitmatch patterns. If the
    expression evaluates to false then the current match case fails to
    match (in other words, we fall through to the next match case - there
    is no error).

    For example:

    bitmatch bits with
    | { field : 16 : check (field > 100) } -> ...

    Note the difference between a check expression and a when-clause
    is that the when-clause is evaluated after all the fields have
    been matched. On the other hand a check expression is evaluated
    after the individual field has been matched, which means it is
    potentially more efficient (if the check expression fails then we
    don't waste any time matching later fields).

*)

(** A bind expression is used to change the value of a matched
    field. For example:

    bitmatch bits with
    | { len : 16 : bind (len * 8);
    field : len : bitstring } -> ...

    In the example, after 'len' has been matched, its value would be
    multiplied by 8, so the width of 'field' is the matched value
    multiplied by 8.

    In the general case:

    | { field : ... : bind (expr) } -> ...

    evaluates the following after the field has been matched: let
    field = expr in (* remaining fields *)
*)

(** Use save_offset_to(variable) to save the current bit offset within
    the match to a variable (strictly speaking, to a pattern). This
    variable is then made available in any check() and bind() clauses
    in the current field, and to any later fields, and to the code
    after the ->.

    For example:

    bitmatch bits with
    | { len : 16;
    _ : len : bitstring;
    field : 16 : save_offset_to (field_offset) } ->
    printf "field is at bit offset %d in the match\n" field_offset

    (In that example, field_offset should always have the value len+16).
*)

(**
   The main concerns for input are buffer overflows and denial of service.

   It is believed that this library is robust against attempted buffer
   overflows. In addition to OCaml's normal bounds checks, we check that
   field lengths are >= 0, and many additional checks.

   Denial of service attacks are more problematic. We only work
   forwards through the bitstring, thus computation will eventually
   terminate. As for computed lengths, code such as this is thought to
   be secure:

   bitmatch bits with
   | { len : 64;
   buffer : Int64.to_int len : bitstring } ->
   
   The len field can be set arbitrarily large by an attacker, but when
   pattern-matching against the buffer field this merely causes a test
   such as if len <= remaining_size to fail. Even if the length is
   chosen so that buffer bitstring is allocated, the allocation of
   sub-bitstrings is efficient and doesn't involve an arbitary-sized
   allocation or any copying.

   However the above does not necessarily apply to strings used in
   matching, since they may cause the library to use the
   Bitstring.string_of_bitstring function, which allocates a
   string. So you should take care if you use the string type
   particularly with a computed length that is derived from external
   input.

   The main protection against attackers should be to ensure that the
   main program will only read input bitstrings up to a certain length,
   which is outside the scope of this library.

   Bitstrings must be loaded into memory before we can match against them.
   Thus available memory may be considered a limit for some applications.
*)


(** A bitstring field of length -1 matches all the rest of the
    bitstring (thus this is only useful as the last field in a
    pattern).

    A bitstring field of length 0 matches an empty bitstring
    (occasionally useful when matching optional subfields).

    Qualifiers are a list of identifiers/expressions which control the
    type, signedness and endianness of the field. Permissible
    qualifiers are:

    int: field has an integer type string: field is a string type
    bitstring: field is a bitstring type signed: field is signed unsigned:
    field is unsigned bigendian: field is big endian - a.k.a network byte
    order littleendian: field is little endian - a.k.a Intel byte order
    nativeendian: field is same endianness as the machine endian (expr):
    expr should be an expression which evaluates to a Bitstring.endian
    type, ie. LittleEndian, BigEndian or NativeEndian. The expression is
    an arbitrary OCaml expression and can use the value of earlier fields
    in the bitmatch.  offset (expr): see computed offsets below.  The
    default settings are int, unsigned, bigendian, no offset.  Note that
    many of these qualifiers cannot be used together, eg. bitstrings do
    not have endianness. The syntax extension should give you a
    compile-time error if you use incompatible qualifiers.
*)
