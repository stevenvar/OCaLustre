
open Parsetree
open Imperative_ast
open Parsing_ast
open Clocking_ast

let get_ident p =
  match p.cp_desc with
  | Ident i -> i
  | _ -> failwith "tuple"

let rec compile_expression e p =
  let compile_preop op =
    match op with
    | Not -> INot
    | Neg -> INeg
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
  match e.ce_desc with
  | CValue v -> IValue v
  | CVariable s -> IVariable s
  | CApplication (i, el) ->
    let iel = List.map (fun e -> compile_expression e p) el in
    IApplication (i, iel)
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
  | CFby (v,e') -> IRef (get_ident p)
  | CWhen (e',i) -> compile_expression e' p
  | _ -> assert false


let generate_inits cnode =
  let generate_init e l =
    match e.cexpression.ce_desc with
    | CFby (v, e') -> ( e.cpattern , IValue v)::l
    | CApplication (i,el) -> (e.cpattern, IApplication (i,[]))::l
    | _ -> l
  in
  List.fold_left (fun acc e -> generate_init e acc) [] cnode.cequations

let init_pre cnode =
  let rec gen_pre name exp l =
    match exp.ce_desc with
    | CFby (v, e') -> gen_pre name e' l
    | _ -> l
  in
  List.fold_left
    (fun acc e -> gen_pre e.cpattern e.cexpression acc)
    []
    cnode.cequations


let generate_updates cnode =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (v,e') -> (eq.cpattern , compile_expression e' eq.cpattern)::l
    | _ -> l
  in
  List.fold_left (fun acc e -> aux e acc) [] cnode.cequations


let compile_equation e =
  let pat = e.cpattern in
  {
    i_pattern = pat;
    i_expression = compile_expression e.cexpression pat;
  }

let compile_io l =
  List.map (fun io -> get_ident io) l

let compile_cnode cnode =

  {
    i_name = get_ident (cnode.cname) ;
    i_inputs = compile_io cnode.cinputs;
    i_outputs = compile_io cnode.coutputs;
    i_inits = generate_inits cnode;
    i_step_fun = {
      i_equations = List.map (compile_equation) cnode.cequations;
      i_updates = generate_updates cnode
    }
  }
