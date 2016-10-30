
open Parsing_ast
open Parsing_ocl
open Error

let new_name , reset =
  let count = ref 0 in
  ( fun () ->
    (incr count;
     let name = ("_aux_"^(string_of_int !count)) in
     name
    )
  ),
  ( fun () -> count := 0 ) 


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
  | Whennot (e,i) ->
    let e' = transform_exp e in
    { exp with e_desc = Whennot (e',i) }
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
    let l',e' = normalize_exp l e in 
    let exp' = { exp with e_desc = Application (i,e') } in
    let (eq_y,y) = new_eq_var exp' in
    let l' = eq_y::l' in
    l', y
  | Call e ->
    l, exp
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
    let (l',c') = normalize_exp l c in
    let (l'',e') = normalize_exp l' e in 
    let exp' = { exp with e_desc = Fby (c',e') } in
    let (eq_y,y) = new_eq_var exp' in
    let l''' = eq_y::l'' in
    l''' , y
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = When (e',i) }
(*TODO*)
  | Whennot (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = Whennot (e',i) }
  | Unit -> l , exp
  | ETuple el ->
    let (l',el') = List.fold_right (fun e (_l,_e) ->
        let (l,e') = normalize_exp _l e in (l@_l,e'::_e)) el (l,[]) in
    l', { exp with e_desc = ETuple el' }
  | Merge (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Merge (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }

let norm_exp l exp  =
  match exp.e_desc with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Alternative (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }
  | Application (i,e) ->
    let l',e' = normalize_exp l e in 
    let exp' = { exp with e_desc = Application (i,e') } in
    l', exp'
  | Call e ->
    l, exp
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
    let (l',c') = normalize_exp l c in
    let (l'',e') = normalize_exp l' e in 
    let exp' = { exp with e_desc = Fby (c',e') } in
    l'' , exp'
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = When (e',i) }
(*TODO*)
  | Whennot (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = Whennot (e',i) }
  | Unit -> l , exp
  | ETuple el ->
    let (l',el') = List.fold_right (fun e (_l,_e) ->
        let (l,e') = normalize_exp _l e in (l@_l,e'::_e)) el (l,[]) in
    l', { exp with e_desc = ETuple el' }
  | Merge (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Merge (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }

let normalize_eqs eqs =
  let normalize_eq eq =
    let exp = transform_exp eq.expression in
    let (new_eqs,new_exp) = norm_exp [] exp in
    { pattern = eq.pattern ; expression = new_exp} ,  new_eqs
  in
  let normalizeed_eqs = List.map normalize_eq eqs in

  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) normalizeed_eqs


let normalize_node node =
  reset (); 
  let (eqs1,eqs2) =  normalize_eqs node.equations in
  {
    pre = node.pre;
    post = node.post;
    inv = node.inv; 
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs2@(List.rev eqs1);
  }
