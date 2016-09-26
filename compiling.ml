
open Parsetree
open Imperative_ast
open Parsing_ast
open Error

let get_num =
  let cpt = ref 0 in
  fun () -> incr cpt; !cpt



let inputs = ref []

let get_ident p =
  match p.p_desc with
  | Ident i -> i
  | _ -> failwith "no tuple"

let rec compile_expression e p =
  let compile_preop op =
    match op with
    | Not -> INot
    | Neg -> INeg
    | Negf -> INegf
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
  match e.e_desc with
  | Value v -> IValue v
  | Variable s -> IVariable s
  | Application (i, e) ->
    let num = get_num () in
    IApplication (i, num, compile_expression e p)
  | InfixOp (op,e1,e2) ->
    IInfixOp(compile_infop op,
             compile_expression e1 p,
             compile_expression e2 p)
  | PrefixOp (op, e) ->
    IPrefixOp (compile_preop op, compile_expression e p)
  | Alternative (e1,e2,e3) ->
    IAlternative (compile_expression e1 p,
                  compile_expression e2 p,
                  compile_expression e3 p)
  | Unit -> IUnit
  | Fby (v,e') -> IRef (get_ident p)
  | When (e',i) -> IAlternative ((compile_expression i p), (compile_expression e' p), (IValue (Nil)))
  | Whennot (e',i) -> IAlternative ((compile_expression i p), IValue (Nil), compile_expression e' p)
  | ETuple el ->
    let iel = List.map (fun e -> compile_expression e p) el in
    IETuple (iel)
  | Merge (e1,e2,e3) ->
    IAlternative (compile_expression e1 p,
                  compile_expression e2 p,
                  compile_expression e3 p)

  | _ -> assert false


let generate_fby_inits el =
  let generate_init e l =
    match e.expression.e_desc with
    | Fby (v, e') -> begin
      match v.e_desc with
      | Value v -> ( e.pattern , IValue v)::l
      | Variable x ->
        if List.mem x !inputs then (e.pattern, IVariable x)::l else
          Error.print_error e.expression.e_loc "A fby must begin with a constant or an input variable"
      | _ -> Error.syntax_error v.e_loc
      end
    | Arrow (e1,e2) -> (e.pattern , compile_expression e1 e.pattern)::l
    | _ -> l
  in
  List.fold_left (fun acc e -> generate_init e acc) [] el

let generate_app_inits el =
let generate_init e l =
  match e.i_expression with
  | IApplication (i,num,el') ->
    (e.i_pattern, IApplication (i,num, el'))::l
  | _ -> l
in
List.fold_left (fun acc e -> generate_init e acc) [] el

let init_pre cnode =
  let rec gen_pre name exp l =
    match exp.e_desc with
    | Fby (v, e') -> gen_pre name e' l
    | _ -> l
  in
  List.fold_left
    (fun acc e -> gen_pre e.pattern e.expression acc)
    []
    cnode.equations


let generate_updates cnode =
  let aux eq l =
    match eq.expression.e_desc with
    | Fby (v,e') -> (eq.pattern , compile_expression e' eq.pattern)::l
    | _ -> l
  in
  List.fold_left (fun acc e -> aux e acc) [] cnode.equations


let compile_equation e =
  let pat = e.pattern in
  {
    i_pattern = pat;
    i_expression = compile_expression e.expression pat;
  }

let rec to_list p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_left (fun acc p -> to_list p @ acc) [] t


let compile_cnode node =
  inputs := to_list node.inputs;
  let i_eqs = List.map (compile_equation) node.equations in
  let i_fby_inits = generate_fby_inits node.equations in
  let i_app_inits = generate_app_inits i_eqs in
  {
    i_name = get_ident (node.name) ;
    i_inputs = node.inputs;
    i_outputs = node.outputs;
    i_inits = i_fby_inits@i_app_inits;
    i_step_fun = {
      i_equations = i_eqs;
      i_updates = generate_updates node
    }
  }
