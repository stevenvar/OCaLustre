(*
* Errors
*)

let print_error loc string =
  Location.print_error Format.std_formatter loc;
  failwith string
    (* raise (Location.Error (Location.error ~loc:loc ("Error:"^string))) *)

let syntax_error loc s =
    print_error loc ("Syntax Error : "^s)
