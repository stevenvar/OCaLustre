
(* Normalisation consists of extracting all the expressions depending on a
state (like ->> or a node call) and putting them in a new, separate, equation.
That way, the "if-then-else" operator keeps its non-lazy semantics when
it is compiled  *)

open Parsing_ast
open Parsing_ocl
open Error

(* names for the new equations  *)
let new_name , reset =
  let count = ref 0 in
  (fun () ->
      incr count;
      let name = ("_aux_"^(string_of_int !count)) in
      name
  ),
  ( fun () -> count := 0 )

(* new variable from new names  *)
let new_eq_var e =
  let name = new_name () in
  let pat = { p_desc = Ident name ; p_loc = Location.none} in
  { pattern = pat  ; expression = e } ,
  { e_desc = Variable name ; e_loc = Location.none }

(* string_of_variable *)
let get_ident e =
  match e.e_desc with
  | Variable v -> v
  | _ -> Format.fprintf Format.std_formatter "%a"
           Parsing_ast_printer.print_expression e ;
    raise @@ Invalid_argument "get_ident"

(* transforms any expression into a normalized expression
 (possibly adding new equation into the environment <l> *)
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
  | Array el -> l,exp
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
  | Pre _ | Arrow _ -> assert false

(* The algorithm begins with this function, because we don't want normalizing
at the first-level
(ex : x = a ->> b; shouldnt create a new equation)  *)

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
  | Array el -> l,exp
  | Fby (c, e) ->
    begin
      match c.e_desc with
      | Value _ | Array _ | Call _ ->
        let (l'',e') = normalize_exp l e in
        let exp' = { exp with e_desc = Fby (c,e') } in
        l'' , exp'
      | _ -> Error.syntax_error c.e_loc "A fby can only have a constant on its left part"
    end
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
  | Arrow _ | Pre _ -> assert false


(* replace every --> and pre with their equivalent with fby *)
  let rec kernalize exp =
    match exp.e_desc with
    | Value _ | Variable _ | Unit | Array _ -> exp
    | Pre e ->
      let e = kernalize e in
      { exp with e_desc = Fby({exp with e_desc = Value Nil}, e) }
    | Arrow (e,e') ->
      let e = kernalize e in
      let e' = kernalize e' in
      let tf = { exp with e_desc = Fby
                              ({ exp with e_desc = Value (Bool true) },
                               { exp with e_desc = Value (Bool false)})
               }
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
     | Application (i,e) ->
       let e' = kernalize e in
       { exp with e_desc = Application (i,e') }
     | Call e ->
       { exp with e_desc = Call e }
     | InfixOp (op,e1,e2) ->
       let e1 = kernalize e1 in
       let e2 = kernalize e2 in
       { exp with e_desc = InfixOp(op,e1,e2) }
     | PrefixOp (op,e) ->
       let e = kernalize e in
       { exp with e_desc = PrefixOp(op,e) }
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
  let kernalize_eq eq =
    let exp = eq.expression in
    { pattern = eq.pattern ; expression = kernalize exp }
  in
  List.map kernalize_eq eqs

(* Normalizing an equation *)
let normalize_eqs eqs =
  let normalize_eq eq =
    let exp = eq.expression in
    let (new_eqs,new_exp) = norm_exp [] exp in
    { pattern = eq.pattern ; expression = new_exp} ,  new_eqs
  in
  let normalized_eqs = List.map normalize_eq eqs in
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) normalized_eqs

(* Entry function: normalize a node *)
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
