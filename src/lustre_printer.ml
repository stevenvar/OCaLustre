open Parsing_ast
open Parsing_ast_printer


let rec print_list f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a; " f h ; print_list f fmt t
  | _ -> ()


let print_infop fmt op =
  match op with
  | Equals -> Format.fprintf fmt "="
  | Plus -> Format.fprintf fmt "+"
  | Times -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Minus -> Format.fprintf fmt "-"
  | Diff -> Format.fprintf fmt "<>"
  | Plusf -> Format.fprintf fmt "+"
  | Timesf -> Format.fprintf fmt "*"
  | Minusf -> Format.fprintf fmt "-"
  | Divf -> Format.fprintf fmt "/"
  | Inf -> Format.fprintf fmt "<"
  | Infe -> Format.fprintf fmt "<="
  | Sup -> Format.fprintf fmt ">"
  | Supe -> Format.fprintf fmt ">="
  | Bor -> Format.fprintf fmt "||"
  | Band -> Format.fprintf fmt "&&"
  | Mod -> Format.fprintf fmt "mod"

let rec print_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "%a" (print_list print_pattern) t
  | PUnit -> Format.fprintf fmt ""
  | Typed (p,s) -> Format.fprintf fmt "%a:%s"
                     print_pattern p
                     s


let rec print_expression fmt e =
  let rec print_expression_list fmt el =
    match el with
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" print_expression e
    | he::te -> Format.fprintf fmt "%a,%a" print_expression he print_expression_list te
  in
  match e.e_desc with
  | Variable i -> Format.fprintf fmt "%a"
                    print_ident i
  | Alternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))"
      print_expression e1
      print_expression e2
      print_expression e3
  | Application (i, e) ->
     Format.fprintf fmt "(%a (%a))"
                    print_ident i
                    print_expression e
  | Call (e) ->
     Format.fprintf fmt "(call ...)"
  | InfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      print_expression e1
      print_infop op
      print_expression e2
  | Pre e -> Format.fprintf fmt "(pre %a)" print_expression e
  | PrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)"
                           print_preop op
                           print_expression e1

  | Value v -> print_value fmt v
  | Arrow (e1,e2) -> Format.fprintf fmt "(%a -> %a)"
                       print_expression e1
                       print_expression e2
  | Fby (e1, e2) -> Format.fprintf fmt "(%a -> pre %a)"
                    print_expression e1
                    print_expression e2
  | Unit -> Format.fprintf fmt "()"
  | When (e1,e2) -> Format.fprintf fmt "( %a when %a )"
                    print_expression e1
                    print_expression e2
  | Whennot (e1,e2) -> Format.fprintf fmt "( %a whennot %a )"
                                      print_expression e1
                                      print_expression e2
  | ETuple el -> Format.fprintf fmt "(%a)"
                   print_expression_list el
  | Merge (e1,e2,e3) ->
    Format.fprintf fmt  "(merge (%a) (%a) (%a))"
    print_expression e1
    print_expression e2
    print_expression e3



let print_equation fmt e =
  Format.fprintf fmt  "  %a = %a;"
    print_pattern e.pattern
    print_expression e.expression

let rec print_equations fmt le =
  match le with
  | [] -> ()
  | e::[] -> Format.fprintf fmt "%a"
               print_equation e
  | e::tl -> Format.fprintf fmt "%a \n%a"
               print_equation e
               print_equations tl

let print_condition fmt cond =
  match cond with
  | Some x ->
    Format.fprintf fmt "\n--%%PROPERTY %a;" print_expression x;
  | None -> ()

let print_node fmt node =
  Format.fprintf fmt "
node %a(%a) returns (%a)
 let
%a%a
 tel
" print_pattern node.name
    print_pattern node.inputs
    print_pattern node.outputs
    print_equations node.equations
    print_condition node.inv
