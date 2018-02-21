(*
* Errors
*)

let print_error loc string =
    raise (Location.Error (Location.error ~loc:loc string))

let syntax_error loc s =
    print_error loc ("Syntax Error : "^s)
