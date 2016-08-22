open Ast

let new_pattern =
  let count = ref 0 in
  fun () -> (incr count;
             { loc = Location.none; content = ("_id"^(string_of_int !count)) }
            )


let new_eq_var e =
  let name = new_pattern () in
  let var = mk_variable (Location.none) name.content in 
  { pattern = name ; expression = e } , var 
 

let rec expand_exp l exp =
  match exp with
  | Alternative (e1,e2,e3) ->
    let (l1,e1') = expand_exp l e1 in
    let (l2,e2') = expand_exp l1 e2 in
    let (l3,e3') = expand_exp l2 e3 in
    l3,Alternative (e1',e2',e3')
  | Application (i,el) -> (l,exp) (*TODO*)
  | InfixOp (op,e1,e2) ->
    let (l1,e1') = expand_exp l e1 in
    let (l2,e2') = expand_exp l1 e2 in
    l2,InfixOp(op,e1',e2')
  | PrefixOp (op,e) ->
    let (l',e') = expand_exp l e in
    (l',PrefixOp(op,e'))
  | Value c -> (l,exp)
  | Variable v -> (l,exp)
  | Fby (c, e) ->
    let (l',e') = expand_exp l e in
    let (eq,var) = new_eq_var (Fby (c,e')) in
    let l' = eq::l' in
    (l',var)
  | When (e,i) -> (l,exp) (*TODO*)
  | Current e -> (l,exp) (*TODO*)
  | Pre v -> (l,exp)
  | Unit -> (l,exp) 

let expand_eqs eqs =
  let expand_eq eq =
    let (new_eqs,new_exp) = expand_exp [] eq.expression in
    { eq with expression = new_exp} ,  new_eqs
  in
  let expanded_eqs = List.map expand_eq eqs in
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) expanded_eqs
  
    
let expand_node node =
  let (eqs1,eqs2) = expand_eqs node.equations in 
  { node with equations = eqs1@eqs2 } 
