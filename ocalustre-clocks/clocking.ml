open Ast

let rec get_clock (e,c) =
  match e with 
  | Variable i -> Base
  | Alternative (e1, e2, e3) -> get_clock e2
  | InfixOp (op, e1, e2) -> get_clock e1
  | PrefixOp (op, e) -> get_clock e
  | Value v -> Base (* TODO : find in a Set *)
  | Fby (v,e) -> get_clock e
  | When (e, i) -> c (* the clock is in the expression *)
  | Unit -> Base 
    

let rec clock_exp (e,c) =
  match e with
  | Variable i -> (e, c)
  | Alternative (e1,e2,e3) ->
    let e = Alternative (clock_exp e1, clock_exp e2, clock_exp e3) in 
    let c1 = get_clock e1 in
    let c2 = get_clock e2 in
    let c3 = get_clock e3 in
    if (c1 = c2 && c2 = c3) then e,c1
    else failwith "Wrong clocks : alternative " (* TODO : better errors *)
  | InfixOp (op, e1, e2) ->
    let e = InfixOp (op, clock_exp e1, clock_exp e2) in 
    let c1 = get_clock e1 in
    let c2 = get_clock e2 in
    if (c1 = c2) then e, c1
    else failwith "Wrong clocks : infixop"
  | PrefixOp (op, e') ->
    let e = PrefixOp (op, clock_exp e') in
    e, (get_clock e')
  | Value v -> e,c (* TODO *)
  | Fby (v,e') ->
    let e = Fby (v, clock_exp e') in
    e,(get_clock e')
  | When (e', i) -> e,c (* nothing to change here *)
  | Unit -> e,c

  
   
let clock_equations el =
  let clock_eq e =
    { pattern = e.pattern ; expression = (clock_exp e.expression) }
  in 
  List.map clock_eq el 

let clock node =
  let eqs = clock_equations node.equations in
  {
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs
  }
