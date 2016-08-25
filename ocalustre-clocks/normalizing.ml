
open Parsing_ast
open Parsing_ocl

let new_name =
  let count = ref 0 in
  fun ck -> (incr count;
             let name = ("_id"^(string_of_int !count)) in
             name
            )


let new_eq_var e =
  let name = new_name () in
  let pat = { p_desc = Ident name ; p_loc = Location.none} in
  { pattern = pat  ; expression = e } ,
  { e_desc = Variable name ; e_loc = Location.none }

let get_ident e =
  match e.e_desc with
  | Variable v -> v
  | _ -> failwith "not an ident"

let rec normalize_exp l exp =
  match exp.e_desc with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Alternative (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }
  | Application (i,el) -> failwith "todo"
  | InfixOp (op,e1,e2) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let exp' = InfixOp (op,e1',e2') in
    l2,{ exp with e_desc =  exp' }
  | PrefixOp (op,e) ->
    let (l',e') = normalize_exp l e in
    let exp' = PrefixOp (op,e') in
    l', { exp with e_desc = exp' }
  | Value c -> l , exp
  | Variable v -> l, exp
  | Fby (c, e) ->
    begin match e.e_desc with
    | Value v -> l, exp
    | _ ->
      let (l',e') = normalize_exp l e in
      let (eq,var) = new_eq_var e' in
      let exp' = { exp with e_desc = Fby (c,var) } in
      let l' = eq::l' in
      l' , exp'
    end
  | Arrow (e1, e2) ->
    let (l1',e1') = normalize_exp l e1 in
    let (l2',e2') = normalize_exp l1' e2 in
    let var_true = { e_desc = Value (Bool true) ; e_loc = e1.e_loc }  in
    let var_false = { e_desc = Value (Bool false) ; e_loc = e1.e_loc }  in
    let tf = Fby (var_true, var_false) in
    let tfe = { e_desc = tf ; e_loc = exp.e_loc } in
      let (eq,var) = new_eq_var tfe in
    let exp' = { exp with e_desc = Alternative (var, e1', e2') } in
    eq::l2', exp'
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = When (e',i) }
  (*TODO*)
  | Pre e ->
    let (l',e') = normalize_exp l e in
    let (eq,var) = new_eq_var e' in
    let magic_exp = { e_desc = Value (Integer 0) ; e_loc = e.e_loc }  in
    let exp' = { exp with e_desc = Fby (magic_exp, var)} in
    let l' = eq::l' in
    l' , exp'
  | Unit -> l , exp

let normalize_eqs eqs =
  let normalize_eq eq =
    let (new_eqs,new_exp) = normalize_exp [] eq.expression in
    { pattern = eq.pattern ; expression = new_exp} ,  new_eqs
  in
  let normalizeed_eqs = List.map normalize_eq eqs in
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) normalizeed_eqs


let normalize_node node =
  let (eqs1,eqs2) = normalize_eqs node.equations in
  {
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs1@eqs2;
  }
