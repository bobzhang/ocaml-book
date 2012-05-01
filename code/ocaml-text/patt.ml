open Format
let rec f = function
| <:re< "foo" (_* as x) "bar" >> :: _ -> Some x
| _ :: l -> f l
| [] -> None  



















