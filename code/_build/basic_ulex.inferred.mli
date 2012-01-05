val __ulex_table_5 : string
val __ulex_table_1 : string
val __ulex_table_2 : string
val __ulex_table_4 : string
val __ulex_table_6 : string
val __ulex_table_7 : string
val __ulex_table_8 : string
val __ulex_table_9 : string
val __ulex_table_10 : string
val __ulex_table_11 : string
val __ulex_table_12 : string
val __ulex_table_13 : string
val __ulex_table_14 : string
val __ulex_table_15 : string
val __ulex_table_16 : string
val __ulex_table_3 : string
val __ulex_partition_5 : int -> int
val __ulex_partition_7 : int -> int
val __ulex_partition_6 : int -> int
val __ulex_partition_16 : int -> int
val __ulex_partition_0 : int -> int
val __ulex_partition_14 : int -> int
val __ulex_partition_11 : int -> int
val __ulex_partition_13 : int -> int
val __ulex_partition_8 : int -> int
val __ulex_partition_9 : int -> int
val __ulex_partition_12 : int -> int
val __ulex_partition_15 : int -> int
val __ulex_partition_3 : int -> int
val __ulex_partition_2 : int -> int
val __ulex_partition_10 : int -> int
val __ulex_partition_17 : int -> int
val __ulex_partition_4 : int -> int
val __ulex_partition_1 : 'a -> int
val lexeme : Ulexing.lexbuf -> string
val basic :
  Ulexing.lexbuf ->
  [> `Lident of string
   | `Lint of int
   | `Lstring of string
   | `Lsymbol of string ]
val token_of_string :
  string ->
  [> `Lident of string
   | `Lint of int
   | `Lstring of string
   | `Lsymbol of string ]
val tokens_of_string : string -> unit
val test_result : OUnit.test_result list
