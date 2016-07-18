open Clocked_ast
open Parsetree
open Ast

type app_inits = (pattern * imp_expr) list
and init = pattern * imp_expr 
and imp_inits = init list 
and imp_expr =
  | IValue of constant
  | IVariable of stream 
  | ITuple of imp_expr list
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IUnit
and
  imp_infop =
  | IEquals
  | IDiff
  | IPlus
  | IMinus
  | ITimes
  | IDiv
  | IPlusf
  | IMinusf
  | IDivf
  | ITimesf
and
  imp_preop =
  | INot

type imp_equation =  {
  i_pattern : pattern;
  i_expression : imp_expr;
} 

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (pattern * imp_expr) list;
}

type imp_node = {
  i_name : ident;
  i_inputs : ident list;
  i_outputs : ident list;
  i_inits : imp_inits;
  i_step_fun : imp_step;
}

let rec compile_expression (e,c) p =
  let compile_preop op =
    match op with
    | Not -> INot
  in
  let compile_infop op =
    match op with
    | Equals -> IEquals
    | Plus -> IPlus
    | Minus -> IMinus
    | Times -> ITimes
    | Div -> IDiv
    | Diff -> IDiff
    | Plusf -> IPlusf
    | Minusf -> IMinusf
    | Timesf -> ITimesf
    | Divf -> IDivf 
  in
  match e with
  | CValue v -> IValue v
  | CVariable (s,c) -> IVariable s
  | CInfixOp (op,e1,e2) ->
    IInfixOp(compile_infop op,
             compile_expression e1 p,
             compile_expression e2 p)
  | CPrefixOp (op, e) ->
    IPrefixOp (compile_preop op, compile_expression e p)
  | CAlternative (e1,e2,e3) ->
    IAlternative (compile_expression e1 p,
                  compile_expression e2 p,
                  compile_expression e3 p)
  | CUnit -> IUnit
  | CFby (v,e') -> IRef p 
  | CWhen (e',i) -> compile_expression e' p
  | CCurrent e' -> compile_expression e' p
  | CPre (v,c) -> IRef v 
                 
let generate_inits cnode =
  let generate_init e l =
    match fst e.cexpression with
    | CFby (v, e') -> ( fst e.cpattern , IValue v)::l
    | _ -> l 
  in 
  List.fold_left (fun acc e -> generate_init e acc) [] cnode.cequations
  
let generate_updates cnode =
  let aux e l =
    match fst e.cexpression with
    | CFby (v,e') -> (fst e.cpattern , compile_expression e' (fst e.cpattern))::l
    | _ -> l 
  in
  List.fold_left (fun acc e -> aux e acc) [] cnode.cequations 

  
let compile_equation e = 
  let pat = fst e.cpattern in
  {
  i_pattern =pat; 
  i_expression = compile_expression e.cexpression pat; 
}

let compile_io l =
  List.map (fun (io,c) -> io) l 

let compile_cnode cnode =
 
  {
    i_name = cnode.cname;
    i_inputs = compile_io cnode.cinputs;
    i_outputs = compile_io cnode.coutputs;
    i_inits = generate_inits cnode;
    i_step_fun = {
      i_equations = List.map (compile_equation) cnode.cequations;
      i_updates = generate_updates cnode }
  }
