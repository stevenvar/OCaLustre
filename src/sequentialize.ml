open Parsing_ast
open Sequential_ast

let rec list_of_pat p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_right (fun p acc -> list_of_pat p @ acc) t []
  | Typed (p',s) -> list_of_pat p'


let seq_eqs_zero eqs =
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
    S_Application (i, seq_exp e)
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


let seq_eqs_next eqs =
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
  let rec seq_exp e p =
  match e.e_desc with
  | Value v -> S_Value v
  | Variable s -> S_Variable s
  | Application (i, e) ->
    S_Application (i, seq_exp e p)
  | Call e ->
    S_Call e
  | InfixOp (op,e1,e2) ->
    S_InfixOp(seq_infop op,
             seq_exp e1 p,
             seq_exp e2 p)
  | PrefixOp (op, e) ->
    S_PrefixOp (seq_preop op, seq_exp e p)
  | Alternative (e1,e2,e3) ->
    S_Alternative (seq_exp e1 p,
                  seq_exp e2 p,
                  seq_exp e3 p)
  | Unit -> S_Unit
  | Arrow (e1,e2) -> seq_exp e2 p
  | Pre e' ->
    begin
      match e'.e_desc with
      | Variable n -> S_Ref n
      | _ -> assert false
    end
  | Fby (e,e') ->
    begin
      match e'.e_desc with
      | Variable n -> S_Ref n
      | _ -> assert false
    end
  | When (e',i) ->
    S_Alternative (seq_exp i p,
                   seq_exp e' p,
                   S_Value Nil)
  | Whennot (e',i) ->
    S_Alternative (seq_exp i p,
                   S_Value Nil,
                   seq_exp e' p)
  | ETuple el ->
    let iel = List.map (fun e -> seq_exp e p) el in
    S_ETuple (iel)
  | Merge (e1,e2,e3) ->
    S_Alternative (seq_exp e1 p,
                   seq_exp e2 p,
                   seq_exp e3 p)
  in
  List.map (fun eq -> { s_pattern = eq.pattern;
                        s_expression = seq_exp eq.expression eq.pattern} ) eqs

module IdentSet = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = ident
  end )

let mk_state n =
  let s = IdentSet.empty in
  let l = List.fold_left (fun acc x -> (list_of_pat x.pattern)@acc ) [] n.equations in
  let s = List.fold_left (fun acc x -> IdentSet.add x acc ) s l in 
  let s = List.fold_left (fun acc x -> IdentSet.add x acc ) s (list_of_pat n.outputs) in
  let s = List.fold_left (fun acc x -> IdentSet.add x acc ) s (list_of_pat n.inputs) in
  IdentSet.elements s
  
  
let seq_zero name inputs outputs state eqs =
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = seq_eqs_zero eqs }

let seq_next name inputs outputs state eqs =
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = seq_eqs_next eqs }

let seq_node n =
  let inputs = list_of_pat n.inputs in
  let outputs = list_of_pat n.outputs in
  let state = mk_state n in 
  let name = n.name in
  { s_name = name; 
    s_zero = seq_zero name inputs outputs state n.equations;
    s_next = seq_next name inputs outputs state n.equations }
