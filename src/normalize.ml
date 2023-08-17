open Parsing_ast

(** Returns a new equation and the corresponding variable **)
let new_eq name e =
  let pat = { p_desc = Ident name ; p_loc = e.e_loc} in
  { pattern = pat; expression = e },
  { e_desc = Variable name ; e_loc = e.e_loc }

let fresh =
  let cpt = ref 0 in
  fun x ->
    incr cpt;
    "_"^x^"_aux"^(string_of_int !cpt)

let norm_exp x e =
  let rec norm_exp decls x exp =
    match exp.e_desc with
    (* k --> pre e => k fby e *)
    | Arrow ({ e_desc = Value _; _} as e1 ,{ e_desc = Pre e ; _}) ->
      let (d,e') = norm_exp decls x e in
      (d, { exp with e_desc = Fby(e1,e') })
    | InfixOp (op,e1,e2) ->
      let (d1,e1') = norm_exp decls x e1 in
      let (d2,e2') = norm_exp d1 x e2 in
      (d2, { exp with e_desc = InfixOp(op,e1',e2') })
    | Fby(k,e) ->
      let x = fresh x in
      let (d,e') = norm_exp decls x e in
      let exp' = { exp with e_desc = Fby(k,e') } in
      let eq',var' = new_eq x exp' in
      (eq'::d,var')
    | Call (f,el)  ->
      let x = fresh x in
      let exp' = { exp with e_desc = Call (f,el) } in
      let eq',var' = new_eq x exp' in
      (eq'::decls,var')
    | Alternative(e1,e2,e3) ->
      let x = fresh x in
      let (d1,e1') = norm_exp decls x e1 in
      let (d2,e2') = norm_exp d1 x e2 in
      let (d3,e3') = norm_exp d2 x e3 in
      (d3, { exp with e_desc = Alternative (e1',e2',e3')})
    | PrefixOp(op,e) ->
      let (d,e') = norm_exp decls x e in
      (d, { exp with e_desc = PrefixOp(op,e') })
    | When(e1,e2) ->
      let (d,e') = norm_exp decls x e1 in
      (d, { exp with e_desc = When(e',e2) })
    | Whennot(e1,e2) ->
      let (d,e') = norm_exp decls x e1 in
      (d, { exp with e_desc = Whennot(e',e2) })
    | Merge(e1,e2,e3) ->
      let x = fresh x in
      let (d1,e1') = norm_exp decls x e1 in
      let (d2,e2') = norm_exp d1 x e2 in
      let (d3,e3') = norm_exp d2 x e3 in
      let exp' = { exp with e_desc = Merge(e1',e2',e3') } in
      let eq',var' = new_eq x exp' in
      (eq'::d3,var')
    | Application (f,n,e) ->
      let name = fresh x in
      let (d,e') = norm_exp decls name e in
      let exp' = { exp with e_desc = Application(f,n,e') } in
      let eq',var' = new_eq name exp' in
      (eq'::d,var')
    | ETuple el ->
      let loop (decls,els) e =
        let (d,e') = norm_exp decls x e in
        (d,e'::els)
      in
      let (d,el) = List.fold_left loop (decls,[]) el in
      (d, {exp with e_desc = ETuple (List.rev el)})
    (* e1 --> e2 => if (true fby false) then e1 else e2 *)
    | Arrow (e1,e2) ->
      let d1,e1' = norm_exp decls x e1 in
      let d2,e2' = norm_exp d1 x e2 in
      let etrue = {exp with e_desc = Value (Bool true)} in
      let efalse = {exp with e_desc = Value (Bool false)} in
      let tf = {exp with e_desc = Fby(etrue, efalse)} in
      let exp' = {exp with e_desc = Alternative(tf,e1',e2') } in
      norm_exp d2 x exp'
    | Pre e ->
      let nil = {exp with e_desc = Value Nil} in
      let (d,e') = norm_exp decls x e in
      (d, { exp with e_desc = Fby(nil,e') })
    | _ -> (decls,exp)
  in
  norm_exp [] x e

let rec norm_eqn eq =
  let rec handle_tuple xs eqs =
    match xs,eqs with
    | [], [] -> []
    | x::xs, exp::exps ->
      let l = norm_eqn {pattern = x; expression = exp } in
      l@(handle_tuple xs exps)
    | _ -> Error.syntax_error eq.expression.e_loc "Wrong number of values"
  in
  match eq.pattern.p_desc with
  |  Ident x -> begin
      match eq.expression.e_desc with
      | Fby (k,e) ->
        let (d,e') = norm_exp x e in
        ({eq with expression = {eq.expression with e_desc = Fby(k,e')}}::d)
      | Merge (e1,e2,e3) ->
        let (d1,e1') = norm_exp x e1 in
        let (d2,e2') = norm_exp x e2 in
        let (d3,e3') = norm_exp x e3 in
        ({eq with expression = {eq.expression with e_desc = Merge(e1',e2',e3')}}::d1@d2@d3)
      | Application (f,n,e) ->
        let (d,e') = norm_exp x e in
        ({eq with expression = {eq.expression with e_desc = Application(f,n,e')}}::d)
      | Call (f,e) ->
        let (d,e') = norm_exp x e in
        ({eq with expression = {eq.expression with e_desc = Call (f,e')}}::d)
      | Pre _ -> Error.syntax_error eq.expression.e_loc
                   "pre can only be on the right hand side of an arrow"
      | _ ->
        let (d,e') = norm_exp x eq.expression in
        ({eq with expression = e'}::d)
    end
  | Tuple xs -> begin
      match eq.expression.e_desc with
      | ETuple eqs -> handle_tuple xs eqs
      | Application (f,n,e) -> let (d,e') = norm_exp f e in
        let new_exp = { e_desc = Application(f,n,e'); e_loc = eq.expression.e_loc} in
        ({eq with expression = new_exp}::d)
      | Call (f,e) ->
        let (d,e') = norm_exp f e in
        let new_exp = { e_desc = Call(f, e'); e_loc = eq.expression.e_loc} in
        ({eq with expression = new_exp}::d)
      | _ -> Error.syntax_error eq.expression.e_loc "expression can only be a tuple or an application"
    end
  | Typed (p,_) -> norm_eqn {eq with pattern = p}
  | PUnit -> assert false (* unreachable *)

let rec norm_eqns eqns =
  match eqns with
  | [] -> []
  | eqn::eqns -> let (eqn') = norm_eqn eqn in
    let (eqns') = norm_eqns eqns in
    (eqn'@eqns')

let norm_node node =
  let (eqs) = norm_eqns node.equations in
  { node with equations = eqs }
