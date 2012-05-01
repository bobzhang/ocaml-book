(* interpolate "string":
 * In the string literal brace expansion is performed. The following
 * notations are allowed:
 * $name                expands to the value of the string variable 'name'
 * $Module.name         expands to the value of the string variable 'name'
 *                      of 'Module'
 * ${name}              same as $name
 * ${Module.name}       same as $Module.name
 * ${name,%format}      expands to the value of the variable 'name' which
 *                      has been converted to a string using "%format".
 *                      The format string may be anything allowed in printf.
 * ${Module.name,%format}    works,too
 * \$                   A dollar character
 * \<newline>           Expands to the empty string
 * All backslash sequences of normal string constants are allowed, too.
 *
 * NOTE: For non-string variables a format specification is required;
 * otherwise type checking is impossible.
 *)

let s = "The number" in
let f = 3.14 in
let i = 42 in
print_string interpolate "$s ${f,%f} is not ${i,%d}\n";;

(**********************************************************************)
(*                     interpolate file                               *)
(**********************************************************************)

(* interpolate file "filename":
 * expands to the contents of the file; brace expansion is performed
 * (see above).
 * If "filename" is written without "/", it is always searched in the
 * same directory as the source file being compiled. Otherwise "filename"
 * is interpreted as relative or absolute path name.
 *
 * IMPORTANT NOTE: Of course, the file is only read during compile time.
 *)

let s = "The number" in
let f = 3.14 in
let i = 42 in
print_string interpolate file "sample.file";;

(**********************************************************************)
(*                        include_file                                *)
(**********************************************************************)

(* include_file "filename":
 * expands to the contents of the file but _no_ brace expansion is performed.
 * If "filename" is written without "/", it is always searched in the
 * same directory as the source file being compiled. Otherwise "filename"
 * is interpreted as relative or absolute path name.
 *
 * IMPORTANT NOTE: Of course, the file is only read during compile time.
 *
 * Note: Up to xstrp4-1.4, this construction used the notation
 * "include file". In xstrp4-1.5, it was changed to "include_file"
 * to avoid ambiguities with O'Caml's built-in "include" directive.
 *)

print_string "sample.file: ";
print_string include_file "sample.file";;

(**********************************************************************)
(*                      <:here< quotations>>                          *)
(**********************************************************************)

(* It is also possible to use the 'here' quotation which does brace
 * expansion on its argument. This is sometimes easier to write because
 * the double quotes need no escaping. Of course, $ and \ characters
 * must still be escaped.
 * Only \$, \<newline>, \>, and \\ are allowed as backslash sequences.
 *)

let s = "The number" in
let f = 3.14 in
let i = 42 in
print_string <:here<\
The interpolation example was:
print_string interpolate "\$s \${f,%f} is not \${i,%d}\n";;
-- where s was replaced by "$s", f by "${f,%f}", and i by "${i,%d}".
>>


(**********************************************************************)
(*                      Access to record fields                       *)
(**********************************************************************)

(* It is also possible to access record fields. Just use the standard
 * OCaml notation rcrd.field.
 *)

type rcrd = 
  {
    s: string;
    f: float;
    i: int;
  }
let rcrd = {s = "The number"; f = 3.14; i = 42} in
print_string interpolate "$rcrd.s ${rcrd.f,%f} is not ${rcrd.i,%d}\n";;





















