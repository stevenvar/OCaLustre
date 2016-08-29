
open Parsing_ast
open Parsing_ocl

let new_name =
  let count = ref 0 in
  fun () ->
    (incr count;
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

let rec transform_exp exp =
  match exp.e_desc with
  | Pre e ->
    let e' = transform_exp e in
    let magic_exp = { e_desc = Value (Magic) ; e_loc = e.e_loc }  in
    { exp with e_desc = Fby (magic_exp, e') }
  | Arrow (e1,e2) ->
    let e1' = transform_exp e1 in
    let e2' = transform_exp e2 in
    let var_true = { e_desc = Value (Bool true) ; e_loc = e1.e_loc }  in
    let var_false = { e_desc = Value (Bool false) ; e_loc = e1.e_loc }  in
    let tfe = { exp with e_desc = Fby (var_true, var_false) } in
    { exp with e_desc = Alternative (tfe, e1', e2') }
  | Alternative (e1,e2,e3) ->
    let e1' = transform_exp e1 in
    let e2' = transform_exp e2 in
    let e3' = transform_exp e3 in
    { exp with e_desc = Alternative (e1', e2', e3') }
  | Application (i,e) ->
    let e' = transform_exp e in
    { exp with e_desc = Application (i,e') }
  | InfixOp (op,e1,e2) ->
    let e1' = transform_exp e1 in
    let e2' = transform_exp e2 in
    { exp with e_desc = InfixOp (op,e1',e2') }
  | PrefixOp (op,e) ->
    let e' = transform_exp e in
    { exp with e_desc = PrefixOp (op,e') }
  | Fby (v,e) ->
    let e' = transform_exp e in
    { exp with e_desc = Fby(v,e') }
  | When (e,i) ->
    let e' = transform_exp e in
    { exp with e_desc = When (e',i) }
  | ETuple el ->
    let el' = List.map transform_exp el in
    { exp with e_desc = ETuple (el') }
  | _ -> exp

let rec normalize_exp l exp =
  match exp.e_desc with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Alternative (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }
  | Application (i,e) ->
    let (l',e') = normalize_exp l e in
    let exp' = Application (i,e') in
    l', { exp with e_desc = exp' }
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
      | _ ->
        let (l',e') = normalize_exp l e in
        let (eq_x,x) = new_eq_var e' in
        let exp' = { exp with e_desc = Fby (c,x) } in
        let (eq_y,y) = new_eq_var exp' in
        let l' = eq_y::eq_x::l' in
        l' , y
    end
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = When (e',i) }
  (*TODO*)
  | Unit -> l , exp
  | ETuple el ->
    let (l',el') = List.fold_right (fun e (_l,_e) -> let (l,e') = normalize_exp _l e in (l@_l,e'::_e)) el (l,[]) in
    l', { exp with e_desc = ETuple el' }
  | _ -> assert false

let normalize_eqs eqs =
  let normalize_eq eq =
    let exp = transform_exp eq.expression in
    let (new_eqs,new_exp) = normalize_exp [] exp in
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
