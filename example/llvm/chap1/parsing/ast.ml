
type expr =
  | Number of float 
  | Variable of string 
  | Binary of char * expr * expr 
  | Call of string * expr array 

type proto = Prototype of string * string array

type func = Function of proto * expr 

(* top ::= definition | external | expression | ';' *)

(* type top = Definition of func | *)
