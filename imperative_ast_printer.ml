open Imperative_ast
open Parsing_ast_printer
open Clocking_ast
open Parsing_ast


let rec print_pattern fmt p =
  match p.cp_desc with
  | CIdent i -> Format.fprintf fmt "%s" i
  | CTuple t -> Format.fprintf fmt "(%a)" (print_list print_pattern) t
  | CPUnit -> Format.fprintf fmt "()"

  let rec printml_tuple fmt l =
    match l with
    | [] -> ()
    | [x] -> Format.fprintf fmt "%a"  print_pattern x
    | h::t -> Format.fprintf fmt "%a,"  print_pattern h; printml_tuple fmt t

let rec printml_expression fmt exp =
  let printml_preop fmt op =
    match op with
    | INot -> Format.fprintf fmt "not "
    | INeg -> Format.fprintf fmt "-"
    | INegf -> Format.fprintf fmt "-."
  in
  let printml_infop fmt op =
    match op with
    | IDiff -> Format.fprintf fmt "<>"
    | IEquals -> Format.fprintf fmt "="
    | IPlus -> Format.fprintf fmt "+"
    | ITimes -> Format.fprintf fmt "*"
    | IDiv -> Format.fprintf fmt "/"
    | IMinus -> Format.fprintf fmt "-"
    | IMinusf -> Format.fprintf fmt "-."
    | IDivf -> Format.fprintf fmt "/."
    | IPlusf -> Format.fprintf fmt "+."
    | ITimesf -> Format.fprintf fmt "*."
  in
  let rec printml_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 printml_expression e
    | e::tl -> Format.fprintf fmt "%a,%a"
                 printml_expression e
                 printml_expressions tl
  in
  match exp with
  | IValue c -> print_value fmt c
  | IVariable v ->  Format.fprintf fmt "%s" v
  | IRef v -> Format.fprintf fmt "!pre_%s" v
  | IInfixOp (op,e1,e2) -> Format.fprintf fmt "%a %a %a"
                             printml_expression e1
                             printml_infop op
                             printml_expression e2
  | IPrefixOp (op,e) -> Format.fprintf fmt "%a%a"
                          printml_preop op
                          printml_expression e
  | IAlternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                 printml_expression e1
                                 printml_expression e2
                                 printml_expression e3
  | IUnit -> Format.fprintf fmt "()"
  | IApplication (i,_,e) -> Format.fprintf fmt "%s (%a)"
                             i
                             printml_expression e
  | IConstr s -> Format.fprintf fmt "%s" s
  | IETuple el -> Format.fprintf fmt "(%a)"
                    printml_expressions el


let printml_updates fmt il =
  let aux fmt (s,e) =
    match e with
    | x -> Format.fprintf fmt "pre_%a := %a;\n"
             print_pattern s
             printml_expression x
  in
  List.iter (fun i -> aux fmt i) il

let printml_equations fmt el =
  let printml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      print_pattern e.i_pattern
      printml_expression e.i_expression
  in
  List.iter (fun x -> printml_equation fmt x) el

(*
let rec printml_io fmt il =
  match il with
  |  [] -> ()
  | s::[] -> Format.fprintf fmt "%s" s.content
  | s::tl -> Format.fprintf fmt "%a,%a"
               printml_string s.content
               printml_io tl
   *)

let printml_step fmt node =
  Format.fprintf fmt "let %s_step (%a) = \n%a%a(%a) \n in %s_step "
    node.i_name
    (print_list print_pattern) node.i_inputs
    printml_equations node.i_step_fun.i_equations
    printml_updates node.i_step_fun.i_updates
    printml_tuple node.i_outputs
    node.i_name


let printml_inits fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | x -> Format.fprintf fmt "let pre_%a = ref %a in\n"
               print_pattern s
               printml_expression x
    end
  in
  List.iter (fun i -> printml_init fmt i) il

let printml_node fmt node =
  Format.fprintf fmt "let %s () =\n%a \n%a \n\n"
    node.i_name
    printml_inits node.i_inits
    printml_step node
    (*
    printml_step node
    printml_string node.i_name.content
*)
