open Ast

let counter =
  let count = ref (0) in
  fun () -> incr count; !count

let isVariable exp = 
  match exp with
  | Variable (i) -> true 
  | _ -> false 

let rec expand_exp e l = 
  match e with 
  | InfixOp (op,e1,e2) -> 
      let (ne1,l1) = expand_exp e1 l in 
      let (ne2,l2) = expand_exp e2 l1 in 
      InfixOp(op,ne1, ne2), l2
  | Variable i -> (e,l)
  | Alternative (e1,e2,e3) ->
    let (ne1,l1) = expand_exp (e1) l in
    let (ne2,l2) = expand_exp (e2) l1 in
    let (ne3,l3) = expand_exp (e3) l2 in 
    Alternative(ne1,ne2,ne3),l3
  | Value v -> e,l
  | Unit -> e,l
  | _ -> e,l

let expand_eq eq =
  let (new_exp, new_eql) = expand_exp eq.expression [] in 
 { eq with expression = new_exp}, new_eql
  
let rec expand_eqs eqs =
  let new_eqs = List.map (fun x -> expand_eq x) eqs in 
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) new_eqs



let rec transform_exp id e l =
  match e with 
  | InfixOp (op, e1, e2) ->
    let ne1 = transform_exp id (e1) l in 
    let ne2 = transform_exp id (e2) l in 
    (InfixOp (op, fst ne1, fst ne2) ) , snd ne1@snd ne2
  | PrefixOp (op, e1) ->
    let ne1 = transform_exp id (e1) l in
        (PrefixOp (op, fst ne1)), (snd ne1)
  | Variable i -> (e,l)
  | Alternative (e1,e2,e3) ->
    let ne1 = transform_exp id (e1) l in
    let ne2 = transform_exp id (e2) l in
    let ne3 = transform_exp id (e3) l in 
    Alternative(fst ne1, fst ne2, fst ne3),(snd ne1@snd ne2@snd ne3)
  | Value v -> e,l
  | Fby (v,e) ->
    let ne = transform_exp id e l in
    Fby(v, fst ne),(snd ne)
  | Unit -> e,l

let transform_eq eq =
  let (new_exp, new_eql) = transform_exp eq.pattern eq.expression [] in 
 { eq with expression = new_exp}, new_eql
  
let rec transform_eqs eqs =
  let new_eqs = List.map (fun x -> transform_eq x) eqs in 
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) new_eqs

let transform_node node = 
  let (ex1,ex2) = expand_eqs node.equations in 
  let eqs = transform_eqs (ex1@ex2) in 
  { node with equations = (fst eqs)@(snd eqs)}
(*
let transform_node node = 
  let eqs = transform_eqs node.equations in 
  { node with equations = (fst eqs)@(snd eqs) }
 *)
