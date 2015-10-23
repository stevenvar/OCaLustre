open Ast




let counter =
  let count = ref (0) in
  fun () -> incr count; !count

let rec transform_exp e l =
 
  let init = mk_ref "init" in
  match e with 
  | InfixOp (op, e1, e2) ->
    begin match op with
      | Arrow -> transform_exp (alternative init e1 e2)
                   ({
                       pattern = mk_pattern "init";
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
        (mk_ref name, {pattern = mk_pattern name ; expression = e}::l) 
      | _ -> let ne1 = transform_exp (e1) l in
        (PrefixOp (op, fst ne1)), (snd ne1)
    end
  | Variable i -> (e,l)
  | Tuple t ->
    let tuplist = List.map (fun e -> transform_exp e l) t in
    let listup =
      List.fold_left (fun l (a,b) -> (a::(fst l),b@(snd l))) ([],[]) tuplist in
    Tuple(fst listup), (snd listup)
  | Ref i -> (e,l)
  | Alternative (e1,e2,e3) ->
    let ne1 = transform_exp (e1) l in
    let ne2 = transform_exp (e2) l in
    let ne3 = transform_exp (e3) l in 
    Alternative(fst ne1, fst ne2, fst ne3),(snd ne1@snd ne2@snd ne3)
  | Application (i,el) -> 
    let tuplist = List.map (fun e -> transform_exp e l) el in
    let (new_eqs, new_list) =
      List.fold_left (fun (l1,l2) (a,b) -> (a::l1,b@l2) ) ([],[]) tuplist in
     
    let name = (i.content^"_step_"^(string_of_int (counter ()) ))in
    let new_exp = { pattern = mk_pattern name ; expression = Application (i,el)} in
     Application (mk_ident name,List.rev new_eqs), new_exp::new_list
  | Value v -> e,l
  | Unit -> e,l

let transform_eq eq =
  let (new_exp, new_eql) = transform_exp eq.expression [] in 
 { eq with expression = new_exp}, new_eql
  
let rec transform_eqs eqs =
  let new_eqs = List.map (fun x -> transform_eq x) eqs in 
  List.fold_left (fun (l1,l2) (x,yl) -> (x::l1),(l2@yl) ) ([],[]) new_eqs

let transform_node node = 
  let eqs = transform_eqs node.equations in 
  { node with equations = (fst eqs)@(snd eqs) }
 
