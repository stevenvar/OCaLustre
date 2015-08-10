open Parsetree
open Asttypes
open Longident
open Ast

type constant = Parsetree.expression 
  
type imp_value =
  | Constant of constant 
  | Variable of string

type imp_init = (string * imp_expr option) list 
and
  imp_expr =
  | IValue of constant
  | IVariable of string
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IApplication of string * imp_expr list
and
  imp_infop =
  | IPlus
  | IMinus
  | ITimes
  | IDiv
and
  imp_preop =
  | IPre
  | INot
    
type imp_equation =  {
  i_pattern : string;
  i_expression : imp_expr;
} 

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (string * imp_value) list;
}

type imp_node = {
  i_name : string;
  i_inputs : string list;
  i_outputs : string list;
  i_inits : imp_init;
  i_step_fun : imp_step;
}

let rec compile_expression exp =
  let compile_preop op =
    match op with
    | Pre -> IPre
    | Not -> INot
  in
  let compile_infop op =
    match op with
    | Plus -> IPlus
    | Minus -> IMinus
    | Times -> ITimes
    | Div -> IDiv
    | _-> failwith "wrong operator" 
  in
  match exp with
  | Value c -> IValue c 
  | Variable v -> IVariable v.content
  | InfixOp (op,e1,e2) -> IInfixOp(compile_infop op,
                                compile_expression e1,
                                   compile_expression e2)
  | PrefixOp (op, e) -> IPrefixOp (compile_preop op, compile_expression e)
  | Alternative (e1,e2,e3) ->  IAlternative (compile_expression e1,
                                             compile_expression e2,
                                             compile_expression e3)
  | Application (id, el) -> IApplication (id.content,
                                          List.map (compile_expression) el)

let printml_string fmt p =
  Format.fprintf fmt "%s" p



let rec printml_expression fmt exp =
  let printml_preop fmt op =
    match op with
    | IPre -> Format.fprintf fmt "mem."
    | INot -> Format.fprintf fmt "not "
  in
  let printml_infop fmt op =
    match op with
    | IPlus -> Format.fprintf fmt " + "
    | ITimes -> Format.fprintf fmt " * "
    | IDiv -> Format.fprintf fmt " / "
    | IMinus -> Format.fprintf fmt " - "
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
  | IValue c -> Pprintast.expression fmt c 
  | IVariable v ->  Format.fprintf fmt "%s" v
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
  | IApplication (s, el) -> Format.fprintf fmt "%a(%a)"
                              printml_string s
                              printml_expressions el

let printml_inits fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | None -> Format.fprintf fmt "let %a = None in\n"
                  printml_string s
      | Some x -> Format.fprintf fmt "let %a = ref %a in\n"
                    printml_string s
                    printml_expression x
   
    end 
  in
  List.map (fun i -> printml_init fmt i) il 
    
let generate_inits node =
  let generate_from_e equation l =
    match equation.expression with
    | PrefixOp (Pre, x) -> (equation.pattern.content,None)::l
    | InfixOp (Arrow, x, y) -> (equation.pattern.content,Some (compile_expression x))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [] equations
      in
  (* list of couples (string, value) *) 
  generate_from_el node.equations

(*
let compile_preop op =
  match op with
  | Pre -> IPre
  | Not -> INot

let compile_infop op =
  match op with
  | Plus -> IPlus
  | Minus -> IMinus
  | Times -> ITimes
  | Div -> IDiv
  | _-> failwith "wrong operator" 
    
let rec compile_expression exp =
  match exp with
  | Value c -> IValue c 
  | Variable v -> IVariable v.content
  | InfixOp (op,e1,e2) -> IInfixOp(compile_infop op,
                                compile_expression e1,
                                   compile_expression e2)
  | PrefixOp (op, e) -> IPrefixOp (compile_preop op, compile_expression e)
  | Alternative (e1,e2,e3) ->  IAlternative (compile_expression e1,
                                             compile_expression e2,
                                             compile_expression e3)
  | Application (id, el) -> IApplication (id.content,
                                          List.map (compile_expression) el)
                         
let compile_equation e = {
  i_pattern = e.pattern.content;
  i_expression = compile_expression e.expression;
}

let compile_equations el =
  List.map (fun x -> compile_equation x) el

let generate_updates el = []
  
let generate_step eql =
  let eqs = compile_equations eql  in
  let ups = generate_updates eql in
  {
    i_equations = eqs;
    i_updates = ups; 
  }

let generate_memory node = {
  i_next_mem =[];
  i_pre_mem = []; 
}

let generate_init node = {
  i_next_init = [];
  i_pre_init = []; 
} 

let compile_inputs l =
  List.map (fun {loc;content} -> content) l
    
let compile_outputs l = compile_inputs l

let compile_node node = {
  i_name = node.name.content;
  i_inputs = compile_inputs node.inputs;
  i_outputs = compile_outputs node.outputs;
  i_memory = generate_memory node;
  i_init_fun = generate_init node;
  i_step_fun = generate_step node.equations; 
}

let printml_preop fmt op =
  match op with
  | IPre -> Format.fprintf fmt "mem."
  | INot -> Format.fprintf fmt "not "
                
let printml_infop fmt op =
  match op with
  | IPlus -> Format.fprintf fmt " + "
  | ITimes -> Format.fprintf fmt " * "
  | IDiv -> Format.fprintf fmt " / "
  | IMinus -> Format.fprintf fmt " - "
                       
let printml_string fmt p =
  Format.fprintf fmt "%s" p


let rec printml_inputs fmt il =
  match il with
  |  [] -> ()
  | s::[] -> Format.fprintf fmt "%s" s
  | s::tl -> Format.fprintf fmt "%a,%a"
               printml_string s
               printml_inputs tl

    
let rec printml_expression fmt exp =
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
  | IValue c -> Pprintast.expression fmt c 
  | IVariable v ->  Format.fprintf fmt "%s" v
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
  | IApplication (s, el) -> Format.fprintf fmt "%a(%a)"
                              printml_string s
                              printml_expressions el
        

let printml_equation fmt e =
  Format.fprintf fmt "let %a = %a in \n"
    printml_string e.i_pattern
    printml_expression e.i_expression


let printml_equations fmt el =
  List.iter (fun x -> printml_equation fmt x) el

let printml_mem_vars fmt vl =
  List.iter (fun x -> Format.fprintf fmt "%s : 'a" x) vl 

let printml_memory fmt node =
  let name = node.i_name in
  let memory = node.i_memory in 
  Format.fprintf fmt "type %a_mem = { \n %a %a } "
    printml_string name
    printml_mem_vars memory.i_next_mem
    printml_mem_vars memory.i_pre_mem

let printml_outputs fmt ol = printml_inputs fmt ol

let printml_init fmt node =
  Format.fprintf fmt "let %a_init = () "
    printml_string node.i_name
                                 
let printml_step fmt node =
  let name = node.i_name in
  let inputs = node.i_inputs in
  let outputs = node.i_outputs in 
  let step = node.i_step_fun in 
  Format.fprintf fmt "let %a_step mem (%a) = \n %a (%a)"
    printml_string name
    printml_inputs inputs
    printml_equations step.i_equations
    printml_outputs outputs
  
let printml_node fmt node =
  Format.fprintf fmt "\n (* NODE %a *)\n\n%a \n%a \n%a \n \n "
    printml_string node.i_name
    printml_memory node 
    printml_init node
    printml_step node 
*)
