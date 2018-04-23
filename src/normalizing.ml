
(** Normalisation consists of extracting all the expressions depending on a
state (like a fby or a node call) and putting them in a new, separate, equation.
That way, the "if-then-else" operator keeps its non-lazy semantics when
it is compiled  **)

open Parsing_ast
open Parsing_ocl
open Error

let check_constant c =
  match c.e_desc with
  | Value _ | Array _ | Call _ -> ()
  | _ ->
    Error.syntax_error c.e_loc "A fby can only have a constant on its left part"

(** names for the new equations  **)
let new_name , reset =
  let count = ref 0 in
  (fun () ->
      incr count;
      let name = ("_aux_"^(string_of_int !count)) in
      name
  ),
  ( fun () -> count := 0 )

(** New variable from new names **)
let new_eq_var e =
  let name = new_name () in
  let pat = { p_desc = Ident name ; p_loc = e.e_loc} in
  { pattern = pat  ; expression = e } ,
  { e_desc = Variable name ; e_loc = e.e_loc }

(** Extract the string in a variable **)
let get_ident e =
  match e.e_desc with
  | Variable v -> v
  | _ ->
    let s = Format.asprintf "Not a variable : %a"
           Parsing_ast_printer.print_expression e in
    Error.print_error e.e_loc s

(** Replace every --> and pre with their equivalent with fby **)
  let rec kernalize exp =
    match exp.e_desc with
    | Value _ | Variable _ | Unit | Array _ -> exp
    | Array_get (e,e') ->
    let e = kernalize e in
    let e' = kernalize e' in
    { exp with e_desc = Array_get(e,e') }
    | Array_fold(e,f,acc) ->
      let e = kernalize e in
      let acc = kernalize acc in
      { exp with e_desc = Array_fold(e,f,acc)}
    | Array_map(e,f) ->
      let e = kernalize e in
      { exp with e_desc = Array_map(e,f) }
    | Imperative_update (e,pe) ->
      let e = kernalize e in
      { exp with e_desc = Imperative_update(e,pe)}
    | Pre e ->
      let e = kernalize e in
      { exp with e_desc = Fby({exp with e_desc = Value Nil}, e) }
    | Arrow (e,e') ->
      let e = kernalize e in
      let e' = kernalize e' in
      let tf = { exp with e_desc = Variable "_init" }
      in
      { exp with e_desc = Alternative(tf,e,e') }
    | Fby(e,e') ->
      let e = kernalize e in
      let e' = kernalize e' in
      { exp with e_desc = Fby(e,e') }
    | Alternative(c,e,e') ->
      let c = kernalize c in
      let e = kernalize e in
      let e' = kernalize e' in
      { exp with e_desc = Alternative(c,e,e') }
     | Application (i,num,e) ->
       let e' = kernalize e in
       { exp with e_desc = Application (i,num,e') }
     | Call e ->
       { exp with e_desc = Call e }
     | InfixOp (op,e1,e2) ->
       let e1 = kernalize e1 in
       let e2 = kernalize e2 in
       { exp with e_desc = InfixOp(op,e1,e2) }
     | PrefixOp (op,e) ->
       let e = kernalize e in
       { exp with e_desc = PrefixOp(op,e) }
     | Clock e ->
       let e = kernalize e in
       { exp with e_desc = Clock e }
     | When (e,i) ->
       let e = kernalize e in
       { exp with e_desc = When (e,i) }
     | Whennot (e,i) ->
       let e = kernalize e in
       { exp with e_desc = Whennot (e,i) }
     | ETuple el ->
       let el = List.map kernalize el in
       { exp with e_desc = ETuple el }
     | Merge (e1,e2,e3) ->
       let e1 = kernalize e1 in
       let e2 = kernalize e2 in
       let e3 = kernalize e3 in
       { exp with e_desc = Merge(e1,e2,e3) }

let kernalize_eqs eqs =
  let init = { pattern = {p_desc = Ident "_init" ; p_loc = Location.none };
      expression =
        { e_desc = Fby
              ({ e_desc = Value (Bool true) ; e_loc = Location.none; } ,
               { e_desc = Value (Bool false) ; e_loc = Location.none; });
          e_loc = Location.none}
    } in
  let kernalize_eq eq =
    let exp = eq.expression in
    { pattern = eq.pattern ; expression = kernalize exp }
  in
  let new_eqs = List.map kernalize_eq eqs in
  if new_eqs <> eqs then
    (* if there is a need for the "init" variable *)
    init::new_eqs
  else eqs


(** Transforms any expression into a normalized expression
    (possibly adding new equation into the environment <l>) **)

let rec normalize_exp l exp =
  match exp.e_desc with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Alternative (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }
  | Application (i,num,e) ->
    let l',e' = normalize_exp l e in
    let exp' = { exp with e_desc = Application (i,num,e') } in
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
  | Array el -> l,exp
  | Array_get (e,e') ->
    let (l,e) = normalize_exp l e in
    let (l,e') = normalize_exp l e' in
    let exp' = Array_get(e,e') in
    l, { exp with e_desc = exp'}
  | Array_fold (e,f,acc) ->
    let (l,e) = normalize_exp l e in
    let (l,e') = normalize_exp l acc in
    let exp' = Array_fold(e,f,e') in
    l, { exp with e_desc = exp'}
  | Array_map (e,f) ->
    let (l,e) = normalize_exp l e in
    let exp' = Array_map(e,f) in
    l, { exp with e_desc = exp'}
  | Imperative_update (e,pe) ->
    let (l,e) = normalize_exp l e in
    let exp' = Imperative_update(e,pe) in
    l, { exp with e_desc = exp' }
  | Fby (c, e) ->
    let (l',c') = normalize_exp l c in
    let (l'',e') = normalize_exp l' e in
    let exp' = { exp with e_desc = Fby (c',e') } in
    let (eq_y,y) = new_eq_var exp' in
    let l''' = eq_y::l'' in
    l''' , y
  | Clock e ->
     let l,e = normalize_exp l e in
     let exp' = { exp with e_desc = Clock e } in
     l,exp'
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    let exp' =  { exp with e_desc = When (e',i) } in
    (* let (eq_y,y) = new_eq_var exp' in *)
    (* let l = eq_y::l' in *)
    l',exp'
    (* l,y *)
  | Whennot (e,i) ->
    let (l',e') = normalize_exp l e in
    let exp' =  { exp with e_desc = Whennot (e',i) } in
    (* let (eq_y,y) = new_eq_var exp' in *)
    (* let l = eq_y::l' in *)
    (* l,y *)
    l',exp'
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
    let exp' = { exp with e_desc =  exp' } in
    let (eq_y,y) = new_eq_var exp' in
    let l = eq_y::l3 in
    l,y
  | Pre e ->
    let l,e = normalize_exp l e in
    let exp' = { exp with e_desc = Pre e } in
    let (eq_y,y) = new_eq_var exp' in
    let l = eq_y::l in
    l,y
  | Arrow (e1,e2) ->
    let (l,e1) = normalize_exp l e1 in
    let (l,e2) = normalize_exp l e2 in
    let exp' = { exp with e_desc = Arrow(e1,e2) } in
    let (eq_y,y) = new_eq_var exp' in
    let l =eq_y::l in
    l,y

(** The algorithm begins with this function, because we don't want normalizing
at the first-level
(ex : x = a fby b; shouldnt create a new equation) **)

let norm_exp l exp  =
  match exp.e_desc with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = normalize_exp l e1 in
    let (l2,e2') = normalize_exp l1 e2 in
    let (l3,e3') = normalize_exp l2 e3 in
    let exp' = Alternative (e1',e2',e3') in
    l3, { exp with e_desc =  exp' }
  | Application (i,num,e) ->
    let l',e' = normalize_exp l e in
    let exp' = { exp with e_desc = Application (i,num,e') } in
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
  | Array el -> l,exp
  | Array_fold (e,f,e') ->
    let (l,e) = normalize_exp l e in
    let (l,e') = normalize_exp l e' in
    let exp' = Array_fold(e,f,e') in
    l , {exp with e_desc = exp'}
  | Array_map (e,f) ->
    let (l,e) = normalize_exp l e in
    let exp' = Array_map(e,f) in
    l, { exp with e_desc = exp' }
  | Array_get (e,e') ->
    let (l,e) = normalize_exp l e in
    let (l,e') = normalize_exp l e' in
    let exp' = Array_get(e,e') in
    l, { exp with e_desc = exp' }
  | Imperative_update (e,pe) ->
    let (l,e) = normalize_exp l e in
    let exp' = Imperative_update(e,pe) in
    l, { exp with e_desc = exp' }
  | Fby (c, e) ->
    let (l'',e') = normalize_exp l e in
    let exp' = { exp with e_desc = Fby (c,e') } in
    l'' , exp'
  | Clock e ->
    let l,e = normalize_exp l e in
    let exp' = { exp with e_desc = Clock e } in
    l,exp'
  | When (e,i) ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = When (e',i) }
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
  | Arrow (e1,e2) ->
    let (l',e1) = normalize_exp l e1 in
    let (l'',e2) = normalize_exp l' e2 in
    let exp' = { exp with e_desc = Arrow (e1,e2) } in
     l'' , exp'
  | Pre e ->
    let (l',e') = normalize_exp l e in
    l' , { exp with e_desc = Pre e' }


(** Normalizing an equation **)
let normalize_eqs eqs =
  let normalize_eq eq =
    let exp = eq.expression in
    let (new_eqs,new_exp) = norm_exp [] exp in
    { pattern = eq.pattern ; expression = new_exp} ,  new_eqs
  in
  let normalized_eqs = List.map normalize_eq eqs in
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) normalized_eqs

(** Normalizing every equation in a node **)
let normalize_node node =
  reset ();
  let eqs = kernalize_eqs node.equations in
  let (eqs1,eqs2) = normalize_eqs eqs in
  {
    pre = node.pre;
    post = node.post;
    inv = node.inv;
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs2@(List.rev eqs1);
  }
