
open Parsetree
open Imperative_ast
open Parsing_ast
open Error
open Scheduler 

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
  | Fby (v,e') -> IRef ("pre_"^(get_ident p))
  | When (e',i) ->
    IAlternative ((compile_expression i p),
                  (compile_expression e' p),
                  (compile_expression e' p))
  | Whennot (e',i) ->
    IAlternative ((compile_expression i p),
                  (compile_expression e' p),
                  (compile_expression e' p))
  | ETuple el ->
    let iel = List.map (fun e -> compile_expression e p) el in
    IETuple (iel)
  | Merge (e1,e2,e3) ->
    IAlternative (compile_expression e1 p,
                  compile_expression e2 p,
                  compile_expression e3 p)

  | _ -> assert false

let rec pre_pattern p =
  let new_desc = match p.p_desc with
    | Ident i -> Ident ("pre_"^i)
    | Tuple t -> Tuple (List.map pre_pattern t)
    | PUnit -> PUnit
  in
  { p with p_desc = new_desc } 

let generate_fby_inits el =
  let generate_init e l =
    match e.expression.e_desc with
    | Fby (v, e') -> (pre_pattern e.pattern, IRefDef (compile_expression v e.pattern))::l
                     (*
      begin
      match v.e_desc with
      | Value v -> ( e.pattern , IValue v)::l
      | _ -> Error.syntax_error v.e_loc
      end
*)
    
    | _ -> l
  in
  List.fold_left (fun acc e -> generate_init e acc) [] el

let generate_app_inits el =
let rec generate_init e p l =
  match e with
  | IApplication (i,num,el') ->
    (p, IApplication (i,num, el'))::l
  | IAlternative (e1,e2,e3) ->
    let l1 = generate_init e1 p l in
    let l2 = generate_init e2 p l1 in
    generate_init e3 p l2
  | _ -> l
in
List.fold_left (fun acc e -> generate_init e.i_expression e.i_pattern acc) [] el

let dep_of_init e l =
  match e.e_desc with
  | Fby _ | Application _  -> Scheduler.get_dep_id e l
  | _ -> l 
  

let generate_inits el =
  let rec generate_init e  l =
    match e.expression.e_desc with
    | Value _
    | Variable _
    | Alternative _
    | Application _ 
    | InfixOp _
    | PrefixOp _
    | When _
    | Whennot _
    | ETuple _
    | Merge _
    | Unit -> (e.pattern, compile_expression e.expression e.pattern)::l                
    | _ -> l
  in
  let exps = List.map (fun e -> e.expression) el in 
  let id_deps = List.fold_left (fun acc e -> dep_of_init e acc) [] exps in
  let e_deps = List.map (fun i -> find_eq_from_id i el) id_deps in
   List.fold_left (fun acc e -> generate_init e acc) [] e_deps

  
      
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
    | Fby (v,e') -> (pre_pattern eq.pattern , compile_expression e' eq.pattern)::l
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
  let i_inits = generate_inits node.equations in 
  let i_fby_inits = generate_fby_inits node.equations in
  let i_app_inits = generate_app_inits i_eqs in
  {
    i_name = get_ident (node.name) ;
    i_inputs = node.inputs;
    i_outputs = node.outputs;
    i_inits = i_inits@i_fby_inits@i_app_inits;
    i_step_fun = {
      i_equations = i_eqs;
      i_updates = generate_updates node
    }
  }
