open Ast




let rec transform_exp e l =
  let init = mk_ref "init" in
  match e with 
  | InfixOp (op, e1, e2) ->
    begin match op with
      | Arrow -> transform_exp (alternative init e1 e2)
                   ({
                       pattern = mk_ident "init";
                       expression = ( mk_expr [%expr true]
                                      --> mk_expr [%expr false]  )
                     }::l)
      | _ ->
        let ne1 = transform_exp (e1) l in 
        let ne2 = transform_exp (e2) l in 
        (InfixOp (op, fst ne1, fst ne2) ) , snd ne1@snd ne2
    end
  | PrefixOp (op, e1) ->
    begin match op with
      | Pre ->
        let idstr = (List.fold_left (fun s x -> s^x.content) "" (get_idents e1))
        in
        let name = "pre_"^idstr in  
        (mk_ref name, {pattern = mk_ident name ; expression = e}::l) 
      | _ -> let ne1 = transform_exp (e1) l in
        (PrefixOp (op, fst ne1)), (snd ne1)
    end
  | Variable i -> (e,l)
  | Ref i -> (e,l)
  | Alternative (e1,e2,e3) ->
    let ne1 = transform_exp (e1) l in
    let ne2 = transform_exp (e2) l in
    let ne3 = transform_exp (e3) l in 
    Alternative(fst ne1, fst ne2, fst ne3),(snd ne1@snd ne2@snd ne3)
  | Application (i,el) -> e,l 
  | Value v -> e,l

let transform_eq eq =
  let (new_exp, new_eql) = transform_exp eq.expression [] in 
 { eq with expression = new_exp}, new_eql
  
let rec transform_eqs eqs =
  let new_eqs = List.map (fun x -> transform_eq x) eqs in 
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) new_eqs

let transform_node node = 
  let eqs = transform_eqs node.equations  in 
  { node with equations = (fst eqs)@(snd eqs) }
 
