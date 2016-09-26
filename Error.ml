(*
* Errors
*)

module Error = struct
  let print_error loc string =
    raise (Location.Error (Location.error ~loc:loc ("Error:"^string)))

  let syntax_error loc =
    print_error loc "Syntax Error"
end
