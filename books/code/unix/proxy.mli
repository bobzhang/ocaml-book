val get_regexp : Util.regexp
val parse_request : string -> string
val proxy_service : Unix.file_descr * 'a -> unit
val proxy : unit -> unit
