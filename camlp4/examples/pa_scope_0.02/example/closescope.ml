let ni = Unix.{ni_hostname = "name"; ni_service = "service"}

module Int32X = struct include Int32 let (+) = add let ( * ) = mul end
let c = Int32X.(3l + 4l * 7l + 6l)

let inline = (struct let x = 3 let y = 4 end).(x + y)
let inline2 = List.(Set.Make(String).(add "inline" empty))

let cons f l = List.(Array.(of_list (hd l)))
