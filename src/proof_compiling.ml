open Proof_ast
open Parsing_ast

let get_num , reset , get =
  let cpt = ref 0 in
  (fun () -> incr cpt; !cpt) , (fun () -> cpt := 0 ) , (fun () -> !cpt)


let rec get_ident p =
  match p.p_desc with
  | Ident i -> i
  | Typed (p,_) -> get_ident p
  | _ -> Parsing_ast_printer.print_pattern Format.std_formatter p;  failwith "-> no tuple  "

let compile_preop op =
  match op with
  | Not -> S_Not
  | Neg -> S_Neg
  | Negf -> S_Negf

let compile_infop op =
  match op with
  | Equals -> S_Equals
  | Plus -> S_Plus
  | Minus -> S_Minus
  | Times -> S_Times
  | Div -> S_Div
  | Diff -> S_Diff
  | Plusf -> S_Plusf
  | Minusf -> S_Minusf
  | Timesf -> S_Timesf
  | Divf -> S_Divf
  | Inf -> S_Inf
  | Infe -> S_Infe
  | Sup -> S_Sup
  | Supe -> S_Supe
  | Bor -> S_Or
  | Band -> S_And
  | Mod -> S_Mod

let rec compile_pre_expression e =
  match e.e_desc with
  | Value v -> S_Value v
  | Array _ | Array_get _ | Imperative_update _ | Array_map _ | Array_fold _ -> failwith "todo"
  | Variable s -> S_Variable ("pre_"^s)
  | Application (i,num, e) ->
    S_Application (i, num, compile_pre_expression e)
  | Call (f,e) ->
    S_Call (f, compile_pre_expression e)
  | InfixOp (op,e1,e2) ->
    S_InfixOp(compile_infop op,
              compile_pre_expression e1,
              compile_pre_expression e2)
  | PrefixOp (op, e) ->
    S_PrefixOp (compile_preop op, compile_pre_expression e)
  | Alternative (e1,e2,e3) ->
    S_Alternative (compile_pre_expression e1 ,
                   compile_pre_expression e2 ,
                   compile_pre_expression e3 )
  | Unit -> S_Unit
  | Arrow (_,_) -> compile_pre_expression e
  | Fby (_,_) -> compile_pre_expression e
  | When (e',i) ->
    S_Alternative (compile_pre_expression i,
                   compile_pre_expression e',
                   S_Value Nil)
  | Whennot (e',i) ->
    S_Alternative (compile_pre_expression i,
                   S_Value Nil,
                   compile_pre_expression e')
  | ETuple el ->
    let iel = List.map (fun e -> compile_pre_expression e) el in
    S_ExpTuple (iel)
  | Pre e -> compile_pre_expression e
  | Merge (e1,e2,e3) ->
    S_Alternative (compile_pre_expression e1,
                   compile_pre_expression e2,
                   compile_pre_expression e3)
  | _ -> invalid_arg "compile_pre_expression"

let rec compile_expression_step e p =
  match e.e_desc with
  | Array _ | Array_get _ | Imperative_update _ | Array_map _ | Array_fold _  -> failwith "todo"
  | Pre _ -> assert false
  | Value v -> S_Value v
  | Variable s -> S_Variable s
  | Application (i,num, e) ->
    S_Application (i, num, compile_expression_step e p)
  | Call (f,e) ->
    S_Call (f, compile_expression_step e p)
  | InfixOp (op,e1,e2) ->
    S_InfixOp(compile_infop op,
              compile_expression_step e1 p,
              compile_expression_step e2 p)
  | PrefixOp (op, e) ->
    S_PrefixOp (compile_preop op, compile_expression_step e p)
  | Alternative (e1,e2,e3) ->
    S_Alternative (compile_expression_step e1 p,
                   compile_expression_step e2 p,
                   compile_expression_step e3 p)
  | Unit -> S_Unit
  | Arrow (_,e2) ->
    compile_pre_expression e2
  | Fby (_,e2) -> compile_pre_expression e2
  | Clock _ -> failwith "I don't work with clocks"
  | When _ -> failwith "I don't work with clocks"
  (* S_Alternative (compile_expression_step i p,
   *               compile_expression_step e' p,
   *               S_Value Nil) *)
  | Whennot (e',i) ->
    S_Alternative (compile_expression_step i p,
                   S_Value Nil,
                   compile_expression_step e' p)
  | ETuple el ->
    let iel = List.map (fun e -> compile_expression_step e p) el in
    S_ExpTuple (iel)
  | Merge (e1,e2,e3) ->
    S_Alternative (compile_expression_step e1 p,
                   compile_expression_step e2 p,
                   compile_expression_step  e3 p)

let compile_expression_init e p =
  match e.e_desc with
  | Fby (e1,_) -> compile_expression_step e1 p
  | _ -> compile_expression_step e p

let rec pre_pattern p =
  let new_desc = match p.p_desc with
    | Ident i -> Ident ("pre_"^i)
    | Tuple t -> Tuple (List.map pre_pattern t)
    | PUnit -> PUnit
    | Typed (p,s) -> Typed(pre_pattern p, s)
  in
  { p with p_desc = new_desc }


let rec s_exp_of_pattern p =
  match p.p_desc with
  | Ident i -> S_Variable i
  | Tuple pl -> S_ExpTuple (List.map s_exp_of_pattern pl)
  | PUnit -> failwith "() is not a good pattern"
  | Typed (p,_) -> s_exp_of_pattern p

let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple pl -> List.fold_left (fun acc p -> (string_of_pattern p)@acc) [] pl
  | PUnit -> []
  | Typed (p,_) -> string_of_pattern p

let generate_updates el =
  let generate_update e l =
    {s_pattern = pre_pattern e.pattern ;  s_expression = (s_exp_of_pattern e.pattern)}::l
  in
  List.fold_left (fun acc e -> generate_update e acc) [] el

let compile_condition c =
  match c with
  | Some x -> Some (compile_expression_step x { p_desc = PUnit ; p_loc = Location.none})
  | None -> None

let rec to_list p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_left (fun acc p -> to_list p @ acc) [] t
  | Typed (p,_) -> to_list p

let compile_equation_step e =
  let pat = e.pattern in
  {
    s_pattern = pat;
    s_expression = compile_expression_step e.expression pat;
  }


let compile_equation_init e =
  let pat = e.pattern in
  {
    s_pattern = pat;
    s_expression = compile_expression_init e.expression pat;
  }

let generate_app_inits el =
  let generate_init e {p_loc; _} l =
    match e.e_desc with
    | Application (i, num,_el') ->
      let p_desc = Ident (i^(string_of_int num)^"_step") in
      {s_pattern = {p_desc ; p_loc} ; s_expression =  S_Application_init (i,S_Unit)}::l
    | _ -> l
  in
  reset ();
  List.fold_left (fun acc e -> generate_init e.expression e.pattern acc) [] el

let rec pat_to_list p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.flatten (List.map pat_to_list t)
  | Typed (p,_) -> pat_to_list p

let rec pat_of_list l =
  let loc = Location.none in
  match l with
  | [] -> { p_desc = PUnit ; p_loc = loc }
  | [x] -> { p_desc = Ident x ; p_loc = loc }
  | _ ->
    let lident = List.map (fun x -> [x]) l in
    let tup = Tuple (List.map (pat_of_list) lident) in
    { p_desc = tup ; p_loc = loc }

let concat_pat p q =
  let p_desc =
    match p.p_desc , q.p_desc with
    | Tuple tp , Tuple tq -> Tuple (tp@tq)
    | Tuple tp , _ -> Tuple (tp@[q])
    | _ , Tuple tq -> Tuple (p::tq)
    | _ , _ -> Tuple ([p;q])
  in
  { p_desc ; p_loc = Location.none }


let rec flatten_pat p =
  match p.p_desc with
  | Tuple (x::t) -> concat_pat x (flatten_pat {p with p_desc = Tuple t})
  | Typed (p',t) -> { p with p_desc = Typed(flatten_pat p',t)}
  | _ -> p


let pcompile_cnode node =
  reset ();
  let s_eqs_step = List.map compile_equation_step node.equations in
  reset ();
  let s_eqs_init = List.map compile_equation_init node.equations in
  let s_app_inits = generate_app_inits node.equations in
  let s_pre = compile_condition node.pre in
  let s_post = compile_condition node.post in
  let s_inv = compile_condition node.inv in
  let s_inputs = node.inputs in
  let s_outputs = node.outputs in
  let s_name = node.name in
  {
    s_pre = compile_condition node.pre;
    s_post = compile_condition node.post;
    s_inv = compile_condition node.inv;
    s_name;
    s_inputs = node.inputs;
    s_outputs = node.outputs;
    s_apps_init = s_app_inits;
    s_init_fun = {
      si_name = s_name;
      si_pre = s_pre;
      si_post = s_post;
      si_inv = s_inv;
      si_inputs = s_inputs;
      si_outputs = s_outputs;
      si_equations = s_eqs_init;
    };
    s_step_fun = {
      ss_name = s_name;
      ss_pre = s_pre;
      ss_post = s_post;
      ss_inv = s_inv;
      ss_inputs = s_inputs;
      ss_outputs = s_outputs;
      ss_equations = s_eqs_step;
    }
  }
