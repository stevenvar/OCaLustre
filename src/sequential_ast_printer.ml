open Sequential_ast
open Parsing_ast_printer
open Clocking_ast
open Parsing_ast

let rec print_s_ident_list fmt l =
  match l with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a"  print_ident x
  | h::t -> Format.fprintf fmt "%a,"  print_ident h; print_s_ident_list fmt t

let rec print_s_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "(%a)" (print_list print_s_pattern) t
  | PUnit -> Format.fprintf fmt "()"
  | Typed (p,s) -> Format.fprintf fmt "(%a:%s)" print_s_pattern p s

  let rec print_s_tuple fmt l =
    match l with
    | [] -> ()
    | [x] -> Format.fprintf fmt "%a"  print_s_pattern x
    | h::t -> Format.fprintf fmt "%a,"  print_s_pattern h; print_s_tuple fmt t

let rec print_s_expression fmt exp =
  let print_s_preop fmt op =
    match op with
    | S_Not -> Format.fprintf fmt "not "
    | S_Neg -> Format.fprintf fmt "-"
    | S_Negf -> Format.fprintf fmt "-."
  in
  let print_s_infop fmt op =
    match op with
    | S_Diff -> Format.fprintf fmt "<>"
    | S_Equals -> Format.fprintf fmt "="
    | S_Plus -> Format.fprintf fmt "+"
    | S_Times -> Format.fprintf fmt "*"
    | S_Div -> Format.fprintf fmt "/"
    | S_Minus -> Format.fprintf fmt "-"
    | S_Minusf -> Format.fprintf fmt "-."
    | S_Divf -> Format.fprintf fmt "/."
    | S_Plusf -> Format.fprintf fmt "+."
    | S_Timesf -> Format.fprintf fmt "*."
    | S_Inf -> Format.fprintf fmt "<"
    | S_Infe -> Format.fprintf fmt "<="
    | S_Sup -> Format.fprintf fmt ">"
    | S_Supe -> Format.fprintf fmt ">="
    | S_Or -> Format.fprintf fmt "||"
    | S_And -> Format.fprintf fmt "&&"
    | S_Mod -> Format.fprintf fmt "mod"
  in
  let rec print_s_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 print_s_expression e
    | e::tl -> Format.fprintf fmt "%a,%a"
                 print_s_expression e
                 print_s_expressions tl
  in
  match exp with
  | S_Value c -> print_value fmt c
  | S_Variable v ->  Format.fprintf fmt "%s" v
  | S_Ref s -> Format.fprintf fmt "state.pre_%s" s
  | S_RefDef e -> Format.fprintf fmt "ref %a" print_s_expression e
  | S_InfixOp (op,e1,e2) -> Format.fprintf fmt "%a %a %a"
                             print_s_expression e1
                             print_s_infop op
                             print_s_expression e2
  | S_PrefixOp (op,e) -> Format.fprintf fmt "%a%a"
                          print_s_preop op
                          print_s_expression e
  | S_Alternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                 print_s_expression e1
                                 print_s_expression e2
                                 print_s_expression e3
  | S_Unit -> Format.fprintf fmt "()"
  | S_Application (i,e) -> Format.fprintf fmt "%s(%a)"
                             i
                             print_s_expression e
  | S_Application_init (i,e) ->
     Format.fprintf fmt "%s (%a)"
                             i
                             print_s_expression e
  | S_Call e -> Format.fprintf fmt "(_____)"
  | S_Constr s -> Format.fprintf fmt "%s" s
  | S_ETuple el -> Format.fprintf fmt "(%a)"
                    print_s_expressions el



let print_s_equations fmt el =
  let print_s_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      print_s_pattern e.s_pattern
      print_s_expression e.s_expression
  in
  List.iter (fun x -> print_s_equation fmt x) el

let rec print_s_inputs fmt ins =
  match ins with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a"  print_ident x
  | x::xs -> Format.fprintf fmt "%a %a" print_ident x print_s_inputs xs

let rec print_s_outputs fmt outs =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a = %a" print_ident x  print_ident x
  | x::xs -> Format.fprintf fmt "%a = %a ; %a" print_ident x print_ident x print_s_outputs xs

let rec print_s_outputs_next fmt outs =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "s.%a <- %a" print_ident x print_ident x
  | x::xs -> Format.fprintf fmt "s.%a <- %a;\n%a"
               print_ident x
               print_ident x
               print_s_outputs_next xs

let print_s_zero fmt f =
  Format.fprintf fmt "let %a_0 %a = \n%a{%a}\n"
    print_s_pattern f.s_name
    print_s_inputs f.s_inputs
    print_s_equations f.s_eqs
    print_s_outputs f.s_outputs

let print_s_next fmt f =
  Format.fprintf fmt "let %a_next %a state = \n%a%a\n"
    print_s_pattern f.s_name
    print_s_inputs f.s_inputs
    print_s_equations f.s_eqs
    print_s_outputs_next f.s_outputs

let print_s_node fmt node =
  Format.fprintf fmt "%a\n%a"
    print_s_zero node.s_zero
    print_s_next node.s_next
