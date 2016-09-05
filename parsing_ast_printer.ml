open Parsing_ast

let print_value fmt v =
  match v with
  | Integer i -> Format.fprintf fmt "%d" i
  | Bool b -> Format.fprintf fmt "%b" b
  | Float f -> Format.fprintf fmt "%f" f
  | Magic -> Format.fprintf fmt "Obj.magic"

let rec print_list f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a, " f h ; print_list f fmt t
  | _ -> ()


let print_ident fmt i = Format.fprintf fmt "%s" i


let rec print_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "(%a)" (print_list print_pattern) t
  | PUnit -> Format.fprintf fmt "()"

let print_io fmt l =
    Format.fprintf fmt "(%a)"
    (print_list (fun fmt io -> print_pattern fmt io)) l

let print_preop fmt op =
  match op with
  | Not -> Format.fprintf fmt "not "
  | Neg -> Format.fprintf fmt "-"
  | Negf -> Format.fprintf fmt "-."


let print_infop fmt op =
  match op with
  | Equals -> Format.fprintf fmt "="
  | Plus -> Format.fprintf fmt "+"
  | Times -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Minus -> Format.fprintf fmt "-"
  | Diff -> Format.fprintf fmt "<>"
  | Plusf -> Format.fprintf fmt "+."
  | Timesf -> Format.fprintf fmt "*."
  | Divf -> Format.fprintf fmt "/."
  | Minusf -> Format.fprintf fmt "-."


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
  | InfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      print_expression e1
      print_infop op
      print_expression e2
  | PrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)"
                           print_preop op
                           print_expression e1

  | Value v -> print_value fmt v
  | Fby (e1, e2) -> Format.fprintf fmt "(%a fby %a)"
                    print_expression e1
                    print_expression e2
  | Arrow (e1, e2) -> Format.fprintf fmt "(%a --> %a)"
                    print_expression e1
                    print_expression e2
  | Unit -> Format.fprintf fmt "()"
  | When (e1,e2) -> Format.fprintf fmt "( %a when %a )"
                    print_expression e1
                    print_expression e2
  | Whennot (e1,e2) -> Format.fprintf fmt "( %a whennot %a )"
                                      print_expression e1
                                      print_expression e2
  | Pre e -> Format.fprintf fmt "(pre %a)"
               print_expression e
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

let print_node fmt n =
  Format.fprintf fmt  "let_node %a ~inf:%a ~outf:%a = \n%a \n \n"
    print_pattern n.name
    print_pattern n.inputs
    print_pattern n.outputs
    print_equations n.equations
