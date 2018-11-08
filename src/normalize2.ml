open Parsing_ast
open Parsing_ocl
open Error


(** Returns a new equation and the corresponding variable **)
let new_eq x e =
  let name = x^"_aux" in
  let pat = { p_desc = Ident name ; p_loc = e.e_loc} in
  { pattern = pat  ; expression = e } ,
  { e_desc = Variable name ; e_loc = e.e_loc }


let rec norm_exp x exp =
  match exp.e_desc with
  | Value k -> ([], exp)
  | Variable v -> ([],exp)
  | InfixOp (op,e1,e2) ->
    let (d1,e1') = norm_exp (x^"1") e1 in
    let (d2,e2') = norm_exp (x^"2") e2 in
    (d1@d2, { exp with e_desc = InfixOp(op,e1',e2') })
  | Fby(k,e) ->
     let (d,e') = norm_exp (x^"_aux") e in
     let exp' = { exp with e_desc = Fby(k,e') } in
     let eq',var' = new_eq x exp' in
     (eq'::d,var')
  |_ -> failwith "not ready"


let rec norm_eqn eq =
  match eq.pattern.p_desc with
  |  Ident x -> begin
      match eq.expression.e_desc with
      | Fby (k,e) -> let (d,e') = norm_exp x (e) in
                     (d,{eq with expression = {eq.expression with e_desc = Fby(k,e')}})
      | e -> let (d,e') = norm_exp x eq.expression in
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

let rec norm_node node =
  let (eqs1,eqs2) = norm_eqns node.equations in
  { node with equations = eqs1@eqs2
  }
