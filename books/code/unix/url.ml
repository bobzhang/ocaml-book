open BatPervasives
open Printf
open Util

let _ = Unix.handle_unix_error (handle_error  Geturl.geturl) ()



















