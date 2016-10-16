
open Parsetree
open Imperative_ast
open Parsing_ast
open Error
open Scheduler

module IdentSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = ident
  end )

let get_num , reset , get =
  let cpt = ref 0 in
  (fun () -> incr cpt; !cpt) , (fun () -> cpt := 0 ) , (fun () -> !cpt)

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
    | Inf -> IInf
    | Infe -> IInfe
    | Sup -> ISup
    | Supe -> ISupe 
  in
  match e.e_desc with
  | Value v -> IValue v
  | Variable s -> IVariable s
  | Application (i, e) ->
    let num = get_num () in
    IApplication (i, num, compile_expression e p)
  | Call e ->
    ICall e
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
  | Fby (v,e') -> IRef ("st_"^(get_ident p))
  | When (e',i) ->
    IAlternative ((compile_expression i p),
                  (compile_expression e' p),
                  IValue Nil)
  | Whennot (e',i) ->
    IAlternative ((compile_expression i p),
                  IValue Nil,
                  (compile_expression e' p))
  | ETuple el ->
    let iel = List.map (fun e -> compile_expression e p) el in
    IETuple (iel)
  | Merge (e1,e2,e3) ->
    IAlternative (compile_expression e1 p,
                  compile_expression e2 p,
                  compile_expression e3 p)

let rec pre_pattern p =
  let new_desc = match p.p_desc with
    | Ident i -> Ident ("st_"^i)
    | Tuple t -> Tuple (List.map pre_pattern t)
    | PUnit -> PUnit
  in
  { p with p_desc = new_desc } 

let generate_fby_inits el =
  let generate_init e l =
    match e.expression.e_desc with
    | Fby (v, e') -> { i_pattern = pre_pattern e.pattern ; i_expression =  IRefDef (compile_expression v e.pattern)}::l
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
    {i_pattern = p ; i_expression =  IApplication (i,num, el')}::l 
  | _ -> l
in
List.fold_left (fun acc e -> generate_init e.i_expression e.i_pattern acc) [] el

(*
let rec get_dep_eq e l el ins : equation list =
  match e.e_desc with
  | Variable v ->
    if List.mem v ins then l else
      let dep = (find_eq_from_id v el) in
      let dep_of_dep = get_dep_eq dep.expression l el ins in
      let deps = dep::(diff el dep_of_dep) in
        deps@l
  | Alternative (e1,e2,e3) ->
    let l1 = get_dep_eq e1 l el ins in
    let l2 = get_dep_eq e2 l1 el ins in
    let l3 = get_dep_eq e3 l2 el ins in 
    l3
  | Application (i,e) ->
    get_dep_eq e l el ins
  | ETuple es ->
    let ids = List.fold_left (fun acc e -> get_dep_eq e acc el ins) l es in
    ids
  | _ -> l  
*)

(*
let dep_of_init e l =
  let deps = match e.e_desc with
    | Fby (e1,_) -> get_dep_id e1 l 
    | Application (i,e)  -> get_dep_id e l 
    | _ -> l
  in
  deps 
*)

let rec get_dep e s el ins =
  match e with
  | IVariable v ->
    if List.mem v ins then s else 
    let eq = (find_ieq_from_id v el).i_expression in
    let deps = get_dep eq s el ins in
    IdentSet.add v deps
  | IAlternative (e1,e2,e3) ->
    let s1 = get_dep e1 s el ins in
    let s2 = get_dep e2 s1 el ins in
    let s3 = get_dep e3 s2 el ins in
    s3
  | IApplication (_,_,e) ->
    get_dep e s el ins
  | ICall _ -> s
  | IRef v -> s
  | IRefDef e -> get_dep e s el ins
  | IInfixOp (_,e1,e2) ->
    let s1 = get_dep e1 s el ins in
    let s2 = get_dep e2 s1 el ins in
    s2
  | IPrefixOp (_,e) ->
    get_dep e s el ins
  | IETuple ee ->
    List.fold_left (fun acc e -> get_dep e acc el ins) s ee
  | _ -> s
  
let generate_inits (el : imp_equation list) inputs  =
  let rec generate_init e s =
    match e with
    | IRefDef e -> get_dep e s el inputs
    | IApplication (_,_,e) -> get_dep e s el inputs
    | _ -> s
  in
  let set_ids = List.fold_left (fun acc e -> generate_init e.i_expression acc) IdentSet.empty el in
  let ids = IdentSet.elements set_ids in
  let ids = diff ids inputs in 
  let eqs = List.map (fun i -> find_ieq_from_id i el) ids in
  eqs

      
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
    | Fby (v,e') -> { i_pattern = pre_pattern eq.pattern ; i_expression = compile_expression e' eq.pattern}::l
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
  reset (); 
  let inputs = to_list node.inputs in 
  let i_eqs = List.map (compile_equation) node.equations in

  let i_inits = generate_inits i_eqs inputs in
  
  let i_fby_inits = generate_fby_inits node.equations in
  let i_app_inits = generate_app_inits i_eqs in
  let i_all_inits = schedule_ieqs (i_inits@i_fby_inits@i_app_inits) inputs in 
  {
    i_name = get_ident (node.name) ;
    i_inputs = node.inputs;
    i_outputs = node.outputs;
    i_inits =  (i_all_inits);
    i_app_inits = [];
    i_fby_inits = []; 
    i_step_fun = {
      i_equations = i_eqs;
      i_updates = generate_updates node
    }
  }
