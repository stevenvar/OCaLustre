open Compiling
open Ast 
open Clocked_ast

(*
let printml_string fmt p =
  Format.fprintf fmt "%s" p
*)

let rec printml_tuple fmt l =
  match l with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%s"  x.content
  | h::t -> Format.fprintf fmt "%s," h.content; printml_tuple fmt t

(*
let printml_pattern fmt p =
  -> printml_string fmt x.content
  
*)

let rec printml_expression fmt exp =
  let printml_preop fmt op =
    match op with
    | INot -> Format.fprintf fmt "not "
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
  | IValue c -> Ast_printer.print_value fmt c
  | ITuple t -> printml_expressions fmt t
  | IVariable v ->  Format.fprintf fmt "%s" v.content
  | IRef v -> Format.fprintf fmt "!pre_%s" v.content
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
  | IApplication (i,el) -> Format.fprintf fmt "%s (%a)"
                             i.content
                             printml_expressions el
  | IConstr s -> Format.fprintf fmt "%s" s
  | IFlow v -> Format.fprintf fmt "Flow.get !%s" v.content

let printml_updates fmt il =
  let aux fmt (s,e) =
    match e with
    | x -> Format.fprintf fmt "pre_%s := %a;\n"
             s.content
             printml_expression x
  in
  List.iter (fun i -> aux fmt i) il

let printml_equations fmt el = 
  let printml_equation fmt e =
    Format.fprintf fmt "let %s = %a in \n"
      e.i_pattern.content
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
    Format.fprintf fmt "let %s_step (%a) = \n%a%a(%a)"
    node.i_name.content
    printml_tuple node.i_inputs
    printml_equations node.i_step_fun.i_equations
    printml_updates node.i_step_fun.i_updates
    printml_tuple node.i_outputs


let printml_inits fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | x -> Format.fprintf fmt "let pre_%s = ref %a in\n"
                    s.content 
                    printml_expression x
    end 
  in
  List.iter (fun i -> printml_init fmt i) il

let printml_node fmt node =
  Format.fprintf fmt "let %s () =\n%a \n%a \n\n"
    node.i_name.content
    printml_inits node.i_inits
    
    printml_step node
    (*
    printml_step node
    printml_string node.i_name.content
*)
