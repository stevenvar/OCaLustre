open Ast
open Astprinter

module S = Set.Make(String)

let rec get_idents e s =
  match e with 
  | Variable i -> S.add i.content s
  | Alternative (e1,e2,e3) ->
    let s = get_idents e1 s in 
    let s = get_idents e2 s in
    get_idents e3 s
  | InfixOp (op, e1, e2) ->
    let s = get_idents e1 s in
    get_idents e2 s
  | PrefixOp (op, e1) ->
    get_idents e1 s
  | Value v -> s

let print_set s =
  S.iter (fun str -> print_string (str^",")) s;
  print_string "\n"

let print_dependencies fmt node =
  let eq_dep fmt eq =
    Format.fprintf fmt "%a depends on "
      print_io eq.pattern;
    print_set (get_idents eq.expression (S.empty))
  in
  List.map (eq_dep fmt) node.equations
