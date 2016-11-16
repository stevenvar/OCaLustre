
open Seq_proof_ast
open Imperative_ast_printer
open Parsing_ast_printer
open Parsing_ast
    open Proof_compiling 


let rec whyml_expression fmt exp =
  let whyml_preop fmt op =
    match op with
    | S_Not -> Format.fprintf fmt "not "
    | S_Neg -> Format.fprintf fmt "-"
    | S_Negf -> Format.fprintf fmt "-."
  in
  let whyml_infop fmt op =
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
  in
  let rec whyml_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 whyml_expression e
    | e::tl -> Format.fprintf fmt "%a,%a"
                 whyml_expression e
                 whyml_expressions tl
  in
  match exp with
  | S_Value c -> print_value fmt c
  | S_Variable v ->  Format.fprintf fmt "%s" v
  | S_Ref v -> Format.fprintf fmt "!%s" v
  | S_RefDef e -> Format.fprintf fmt "ref %a" whyml_expression e
  | S_InfixOp (op,e1,e2) -> Format.fprintf fmt "%a %a %a"
                             whyml_expression e1
                             whyml_infop op
                             whyml_expression e2
  | S_PrefixOp (op,e) -> Format.fprintf fmt "%a%a"
                          whyml_preop op
                          whyml_expression e
  | S_Alternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                 whyml_expression e1
                                 whyml_expression e2
                                 whyml_expression e3
  | S_Unit -> Format.fprintf fmt "()"
  | S_Application (i,num,e) -> Format.fprintf fmt "%s%d_step (%a)"
                              i
                              num
                             whyml_expression e
  | S_Application_init (i,e) ->
     Format.fprintf fmt "%s (%a)"
                             i
                             whyml_expression e
  | S_Call e -> Format.fprintf fmt "(_____)"
  | S_Constr s -> Format.fprintf fmt "%s" s
  | S_ExpTuple el -> Format.fprintf fmt "(%a)"
                    whyml_expressions el

let whyml_equations fmt el =
  let whyml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      print_pattern e.s_pattern
      whyml_expression e.s_expression
  in
  List.iter (fun x -> whyml_equation fmt x) el


let print_pre fmt p =
  match p with
  | None -> ()
  | Some x ->
    Format.fprintf fmt "requires { %a } \n" whyml_expression x         

let print_post fmt (p,outs) =
  match p with
  | None -> ()
  | Some x ->
    Format.fprintf fmt "ensures { let %a = result in %a } \n"
      print_pattern
      outs
      whyml_expression x         

let get_patterns el =
  List.map (fun e -> e.s_pattern) el 

let whyml_fun_init fmt (name,inputs,pre,post,s_init,outputs) =
  let pats = get_patterns s_init.s_init_equations in
  let tpats =
    match pats with
    | [x] -> x
    | _ -> { p_desc = Tuple pats ; p_loc = Location.none }  in 
  Format.fprintf fmt "let %s_init %a = \n%a%a%a in \n let st=(%a,%a) in \n (st,%a)"
  name
  print_pattern inputs
  print_pre pre
  print_post (post,outputs)
  whyml_equations s_init.s_init_equations
  print_pattern tpats
  print_pattern inputs
  print_pattern outputs

let whyml_fun_step fmt (name,inputs,pre,post,s_step,outputs) =
  let pats = get_patterns s_step.s_step_equations in
  let tpats =
    match pats with
    | [x] -> x
    | _ -> { p_desc = Tuple pats ; p_loc = Location.none }  in 
  Format.fprintf fmt "let %s_step (%a,%a) %a = \n%a%a%a in \n let st=(%a,%a) in \n (st,%a)"
    name
    print_pattern (pre_pattern tpats)
    print_pattern (pre_pattern inputs) 
  print_pattern inputs
  print_pre pre
  print_post (post,outputs)
  whyml_equations s_step.s_step_equations
  print_pattern tpats
  print_pattern inputs
  print_pattern outputs

let whyml_node fmt node =
 
  Format.fprintf fmt "let %s () = \n  APP INITS \n %a \n \n and \n %a "
  node.s_name
  whyml_fun_init 
      (node.s_name
      ,node.s_inputs
      ,node.s_pre
      ,node.s_post
      ,node.s_init_fun
      ,node.s_outputs)
  whyml_fun_step 
      (node.s_name
      ,node.s_inputs
      ,node.s_pre
      ,node.s_post
      ,node.s_step_fun
      ,node.s_outputs)
   
   
