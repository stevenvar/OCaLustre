open Ast_imperative
open Ast 
open Ast_clock

let printml_string fmt p =
  Format.fprintf fmt "%s" p



let rec printml_tuple fmt l =
  match l with
  | [] -> ()
  | [x] -> printml_string fmt x.content
  | h::t -> printml_string fmt (h.content^",") ; printml_tuple fmt t

let printml_pattern fmt p =
  match p with
  | C_Simple (x,c) -> printml_string fmt x.content
  | C_List t -> printml_tuple fmt (List.map fst t)


let rec printml_expression fmt exp =
  let printml_preop fmt op =
    match op with
    | IPre -> Format.fprintf fmt "pre "
    | INot -> Format.fprintf fmt "not "
  in
  let printml_infop fmt op =
    match op with
    | IDiff -> Format.fprintf fmt " <> "
    | ILess -> Format.fprintf fmt " < "
    | ILesse -> Format.fprintf fmt " <= "
    | IGreat -> Format.fprintf fmt " > "
    | IGreate -> Format.fprintf fmt " >= "
    | IEquals -> Format.fprintf fmt " = "  
    | IPlus -> Format.fprintf fmt " + "
    | ITimes -> Format.fprintf fmt " * "
    | IDiv -> Format.fprintf fmt " / "
    | IMinus -> Format.fprintf fmt " - "
    | IPlusf -> Format.fprintf fmt " +. "
    | ITimesf -> Format.fprintf fmt " *. "
    | IDivf -> Format.fprintf fmt " /. "
    | IMinusf -> Format.fprintf fmt " - "
    | IAnd -> Format.fprintf fmt " and "
    | IOr -> Format.fprintf fmt " or "
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
  | IRef v -> Format.fprintf fmt "Option.get (!%s)" v.content
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
  | IApplication (s, el) -> Format.fprintf fmt "%a (%a)"
                              printml_string s.content
                              printml_expressions el
  | IApplication_init (s, el) -> Format.fprintf fmt "%a (%a)"
                              printml_string s.content
                              printml_expressions el
  | IUnit -> Format.fprintf fmt " () "
  | IWhen (e,i) -> Format.fprintf fmt " %a when %a "
      printml_expression e 
      printml_string i.content 
  | ICall e -> Format.fprintf fmt " CALL XXX "

let printml_inits fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | None -> Format.fprintf fmt "let %a = ref None in\n"
                  printml_pattern s
      | Some x -> Format.fprintf fmt "let %a = ref (Some %a) in\n"
                    printml_pattern s
                    printml_expression x
    end 
  in
  List.iter (fun i -> printml_init fmt i) il

let printml_updates fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | None -> Format.fprintf fmt "%a := None;\n"
                  printml_pattern s
      | Some x -> Format.fprintf fmt "%a := (Some %a);\n"
                    printml_pattern s
                    printml_expression x
    end 
  in
  List.iter (fun i -> printml_init fmt i) il

let printml_equations fmt el = 
  let printml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      printml_pattern e.i_pattern
      printml_expression e.i_expression
  in
  List.iter (fun x -> printml_equation fmt x) el
  
let rec printml_io fmt il =
  match il with
  |  [] -> ()
  | s::[] -> Format.fprintf fmt "%s" s.content
  | s::tl -> Format.fprintf fmt "%a,%a"
               printml_string s.content
               printml_io tl
               
let printml_step fmt node =
  let name = node.i_name in
  let inputs = node.i_inputs in
  let outputs = node.i_outputs in 
  let step = node.i_step_fun in 
  Format.fprintf fmt "let %a_step (%a) = \n %a %a (%a)"
    printml_string name.content
    printml_io inputs
    printml_equations step.i_equations
    printml_updates step.i_updates
    printml_io outputs
    
let printml_node fmt node =
  Format.fprintf fmt "\n let %a ()  =\n%a %a \nin %a_step \n\n"
    printml_string node.i_name.content
    printml_inits node.i_inits
    printml_step node
    printml_string node.i_name.content
