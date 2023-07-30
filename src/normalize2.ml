open Parsing_ast

(** Returns a new equation and the corresponding variable **)
let new_eq x e =
  let name = x in
  let pat = { p_desc = Ident name ; p_loc = e.e_loc} in
  { pattern = pat  ; expression = e } ,
  { e_desc = Variable name ; e_loc = e.e_loc }

let cpt = ref 0
let fresh x =
  incr cpt;
  "_"^x^"_aux"^(string_of_int !cpt)


let rec norm_exp x exp =
  match exp.e_desc with
  | InfixOp (op,e1,e2) ->
    let (d1,e1') = norm_exp x e1 in
    let (d2,e2') = norm_exp x e2 in
    (d1@d2, { exp with e_desc = InfixOp(op,e1',e2') })
  | Fby(k,e) ->
    let x = fresh x in
    let (d,e') = norm_exp x e in
    let exp' = { exp with e_desc = Fby(k,e') } in
    let eq',var' = new_eq x exp' in
    (eq'::d,var')
  | Call (f,el)  ->
    let x = fresh x in
    let exp' = { exp with e_desc = Call (f,el) } in
    let eq',var' = new_eq x exp' in
    ([eq'],var')
  | Alternative(e1,e2,e3) ->
    let x = fresh x in
    let (d1,e1') = norm_exp x e1 in
    let (d2,e2') = norm_exp x e2 in
    let (d3,e3') = norm_exp x e3 in
    let exp' = { exp with e_desc = Alternative (e1',e2',e3') } in
    let eq',var' = new_eq x exp' in
    (eq'::d1@d2@d3,var')
  | PrefixOp(op,e) ->
    let (d,e') = norm_exp x e in
    (d, { exp with e_desc = PrefixOp(op,e') })
  | When(e1,e2) ->
    let (d,e') = norm_exp x e1 in
    (d, { exp with e_desc = When(e',e2) })
  | Whennot(e1,e2) ->
    let (d,e') = norm_exp x e1 in
    (d, { exp with e_desc = Whennot(e',e2) })
  | Merge(e1,e2,e3) ->
    let x = fresh x in
    let (d1,e1') = norm_exp x e1 in
    let (d2,e2') = norm_exp x e2 in
    let (d3,e3') = norm_exp x e3 in
    let exp' = { exp with e_desc = Merge(e1',e2',e3') } in
    let eq',var' = new_eq x exp' in
    (eq'::d1@d2@d3,var')
  | Application (f,n,e) ->
    let x = fresh x in
    let (d,e') = norm_exp x e in
    let exp' = { exp with e_desc = Application(f,n,e') } in
    let eq',var' = new_eq x exp' in
    (eq'::d,var')
  | ETuple (el) ->
    let (d,el) = List.fold_left (fun (ds',es') e ->
        let (d',e') = norm_exp x e in
        (d'@ds', e'::es')) ([],[]) el
    in
    (d,{ exp with e_desc = ETuple (List.rev el) } )
  | _ -> ([],exp)


let rec norm_eqn eq =
  match eq.pattern.p_desc with
  |  Ident x -> begin
      match eq.expression.e_desc with
      | Fby (k,e) -> let (d,e') = norm_exp x (e) in
        (d,{eq with expression = {eq.expression with e_desc = Fby(k,e')}})
      | Application (f,n,e) -> let (d,e') = norm_exp x (e) in
        (d,{eq with expression = {eq.expression with e_desc = Application(f,n,e')}})
      | Call (f,el) ->
        let (d,e') = List.fold_left (fun (d,es) e ->
            let (_d,_e) = norm_exp x e in
            (d@_d,_e::es)) ([],[]) el in
        (d,{eq with expression = {eq.expression with e_desc = Call (f,List.rev e')}})
      | _ -> let (d,e') = norm_exp x eq.expression in
        (d,{eq with expression = e'})
    end
  | Tuple [x] -> begin
      match eq.expression.e_desc with
      | ETuple [e] -> let (d,e') = norm_eqn {pattern = x; expression = e} in
        (d,e')
      | _ -> failwith "nil e"
    end
  | Tuple (x::xs) -> begin
      match eq.expression.e_desc with
      | ETuple (e::es) -> let (d,e') = norm_eqn {pattern = x; expression = e} in
        let pat = { eq.pattern with p_desc = Tuple xs } in
        let exp = { eq.expression with e_desc = ETuple es } in
        let (ds,es') =  norm_eqn {pattern = pat; expression = exp } in
        (ds@d@[es'],e')
      | Application (f,n,e) -> let (d,e') = norm_exp f (e) in
        (d,{eq with expression = {eq.expression with e_desc = Application(f,n,e')}})
      | Call _ -> ([],eq)
      | _ -> failwith "nope"
    end
  | Tuple [] -> failwith "nil"
  | PUnit -> failwith "punit"
  | Typed _ -> failwith "types"



let rec norm_eqns eqns =
  match eqns with
  | [] -> [],[]
  | eqn::eqns -> let (d,eqn') = norm_eqn eqn in
    let (d',eqns') = norm_eqns eqns in
    (d@d',eqn'::eqns')

let norm_node node =
  let (eqs1,eqs2) = norm_eqns node.equations in
  { node with equations = eqs1@eqs2
  }
