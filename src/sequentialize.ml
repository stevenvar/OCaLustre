open Parsing_ast
open Sequential_ast

let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> i
  | _ -> assert false

let rec list_of_pat p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_right (fun p acc -> list_of_pat p @ acc) t []
  | Typed (p',s) -> list_of_pat p'

let rec mk_outputs name outs =
  let rec loop outs = 
    match outs with
    | [] -> []
    | x::xs -> S_Variable (name^"_out_"^x) :: loop xs 
  in
  S_ETuple (loop outs) 

let seq_eqs_zero eqs env =
  let seq_preop op =
    match op with
    | Not -> S_Not
    | Neg -> S_Neg
    | Negf -> S_Negf
  in
  let seq_infop op =
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
  in
  let rec seq_exp e =
  match e.e_desc with
  | Value v -> S_Value v
  | Variable s -> S_Variable s
  | Application (i, e) ->
    (try
      let outputs = List.assoc i !env in
      mk_outputs (i^"_state."^i) outputs
    with Not_found -> failwith ("unknown node "^i))
    (* S_Application_init (i^"_0", seq_exp e) *)
  | Call e ->
    S_Call e
  | InfixOp (op,e1,e2) ->
    S_InfixOp(seq_infop op,
             seq_exp e1,
             seq_exp e2)
  | PrefixOp (op, e) ->
    S_PrefixOp (seq_preop op, seq_exp e)
  | Alternative (e1,e2,e3) ->
    S_Alternative (seq_exp e1,
                  seq_exp e2,
                  seq_exp e3)
  | Unit -> S_Unit
  | Arrow (e1,e2) -> seq_exp e1
  | Pre e -> assert false
  | Fby (e,e') -> seq_exp e
  | When (e',i) ->
    S_Alternative (seq_exp i,
                   seq_exp e',
                   S_Value Nil)
  | Whennot (e',i) ->
    S_Alternative (seq_exp i,
                   S_Value Nil,
                   seq_exp e')
  | ETuple el ->
    let iel = List.map (fun e -> seq_exp e) el in
    S_ETuple (iel)
  | Merge (e1,e2,e3) ->
    S_Alternative (seq_exp e1,
                   seq_exp e2,
                   seq_exp e3)
  in
  List.map (fun eq -> { s_pattern = eq.pattern;
                        s_expression = seq_exp eq.expression} ) eqs

let rec seq_exp_list e =
     let seq_preop op =
    match op with
    | Not -> S_Not
    | Neg -> S_Neg
    | Negf -> S_Negf
  in
  let seq_infop op =
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
  in
    match e.e_desc with
  | Value v -> S_Value v
  | Variable s -> S_Variable s
  | Application (i, e) ->
    S_Application (i^"_next", seq_exp_list e)
  | Call e ->
    S_Call e
  | InfixOp (op,e1,e2) ->
    S_InfixOp(seq_infop op,
             seq_exp_list e1,
             seq_exp_list e2)
  | PrefixOp (op, e) ->
    S_PrefixOp (seq_preop op, seq_exp_list e)
  | Alternative (e1,e2,e3) ->
    S_Alternative (seq_exp_list e1,
                  seq_exp_list e2,
                  seq_exp_list e3)
  | Unit -> S_Unit
  | Arrow (e1,e2) -> seq_exp_list e2
  | Pre e' ->
    begin
      match e'.e_desc with
      | Variable n -> S_Ref ("pre_"^n)
      | _ -> assert false
    end
  | Fby (e,e') ->
    begin
      match e'.e_desc with
      | Variable n -> S_Ref ("pre_"^n)
      | _ -> assert false
    end
  | When (e',i) ->
    S_Alternative (seq_exp_list i,
                   seq_exp_list e',
                   S_Value Nil)
  | Whennot (e',i) ->
    S_Alternative (seq_exp_list i,
                   S_Value Nil,
                   seq_exp_list e')
  | ETuple el ->
    let iel = List.map (fun e -> seq_exp_list e) el in
    S_List (iel)
  | Merge (e1,e2,e3) ->
    S_Alternative (seq_exp_list e1,
                   seq_exp_list e2,
                   seq_exp_list e3)

let rec seq_eqs_next eqs name env =
   let seq_preop op =
    match op with
    | Not -> S_Not
    | Neg -> S_Neg
    | Negf -> S_Negf
  in
  let seq_infop op =
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
  in
  let rec seq_exp e = 
  match e.e_desc with
  | Value v -> S_Value v
  | Variable s -> S_Variable s
  | Application (i, e) ->
      (try
      let outputs = List.assoc i !env in
      mk_outputs ("state."^name^"_"^i^"_state."^i) outputs
    with Not_found -> failwith ("unknown node "^i))
  | Call e ->
    S_Call e
  | InfixOp (op,e1,e2) ->
    S_InfixOp(seq_infop op,
             seq_exp e1,
             seq_exp e2)
  | PrefixOp (op, e) ->
    S_PrefixOp (seq_preop op, seq_exp_list e)
  | Alternative (e1,e2,e3) ->
    S_Alternative (seq_exp e1,
                  seq_exp e2,
                  seq_exp e3)
  | Unit -> S_Unit
  | Arrow (e1,e2) -> seq_exp e2
  | Pre e' ->
    begin
      match e'.e_desc with
      | Value v -> S_Value v 
      | Variable n -> S_Ref ("pre_"^n)
      | _ -> assert false
    end
  | Fby (e,e') ->
    begin
      match e'.e_desc with
      | Value v -> S_Value v
      | Variable n -> S_Ref ("pre_"^n)
      | _ -> assert false
    end
  | When (e',i) ->
    S_Alternative (seq_exp i,
                   seq_exp e',
                   S_Value Nil)
  | Whennot (e',i) ->
    S_Alternative (seq_exp i,
                   S_Value Nil,
                   seq_exp e')
  | ETuple el ->
    let iel = List.map (fun e -> seq_exp e) el in
    S_ETuple (iel)
  | Merge (e1,e2,e3) ->
    S_Alternative (seq_exp e1,
                   seq_exp e2,
                   seq_exp e3)
  in
  List.map (fun eq -> { s_pattern = eq.pattern;
                        s_expression = seq_exp eq.expression} ) eqs

let call_state e l name =
  match e.e_desc with
  | Application (i, e) ->
    { s_pattern = Parsing_ocl.mk_pattern (i^"_state");
      s_expression = S_Application_init (i^"_0", seq_exp_list e)}
    ::l
  | _ -> l

let call_state_next e l name =
  match e.e_desc with
  | Application (i, e) ->
    { s_pattern = Parsing_ocl.mk_pattern "_";
      s_expression = S_Application (i, seq_exp_list e)}
    ::l
  | _ -> l


let state_eq e ({pres;calls;outs} as s) =
  match e.e_desc with
  | Application (i, e) ->
    { s with calls = i::calls }
  | Pre e' ->
    begin
      match e'.e_desc with
      | Value v -> s
      | Variable n -> { s with pres = n::pres }
      | _ -> assert false
    end
  | Fby (e,e') ->
    begin
      match e'.e_desc with
      | Value v -> s 
      | Variable n -> { s with pres = n::pres }
      | _ -> assert false
    end
  | _ -> s


module IdentSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = ident
  end )

let mk_state n =
  let s = { pres = []; calls = []; outs = (list_of_pat n.outputs) } in
  let l = List.fold_left
      (fun acc eq -> state_eq eq.expression acc) s n.equations in
  l


let seq_zero name inputs outputs state env eqs =
  let sname = string_of_pattern name in 
  let call_init =
    List.fold_left
      (fun acc eq -> call_state eq.expression acc sname) [] eqs in
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = call_init @ (seq_eqs_zero eqs env) }

let seq_next name inputs outputs state env eqs =
  let sname = string_of_pattern name in 
  let call_init =
    List.fold_left
      (fun acc eq -> call_state_next eq.expression acc sname) [] eqs in
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = call_init @ (seq_eqs_next eqs sname env) }

let seq_node n env =
  let inputs = list_of_pat n.inputs in
  let outputs = list_of_pat n.outputs in
  let state = mk_state n in
  let name = n.name in
  let sname = string_of_pattern name in 
  env := (sname,outputs)::!env;
  { s_name = name;
    s_zero = seq_zero name inputs outputs state env n.equations;
    s_next = seq_next name inputs outputs state env n.equations }