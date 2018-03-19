open Parsing_ast
open Clocking_ast

let clock_e gamma e =
  let open Clocking_ocl in
  let tau = clock_expr gamma e in
  let tau = shorten tau in
  tau

let clock_expr gamma (e:expression) =
  let rec clock_rec e =
    match e.e_desc with
    | Value v ->
        let clk = Clocking_ocl.clock_expr gamma e in
       { ce_desc = CValue v ; ce_loc = e.e_loc ; ce_clock = clk}
    | Unit ->
       let clk = Clocking_ocl.clock_expr gamma e in
       { ce_desc = CUnit ; ce_loc = e.e_loc ; ce_clock = clk}
    | Array e' ->
       let e' = List.map clock_rec e' in
       { ce_desc = CArray e' ; ce_loc = e.e_loc; ce_clock = (List.hd e').ce_clock}
    | Array_get (e',n) ->
       let e' =  clock_rec e in
       let n =  clock_rec n in
       { ce_desc = CArray_get (e',n) ; ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Array_fold (e',f,acc) ->
      let e' = clock_rec e' in
      let acc = clock_rec acc in
      Clocking_ocl.unify(e'.ce_clock,acc.ce_clock);
      { ce_desc = CArray_fold(e',f,acc); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Array_map (e',f) ->
       let e' = clock_rec e' in
      { ce_desc = CArray_map(e',f); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Imperative_update (e',up) ->
       let e'= clock_rec e in
       let up = List.map (fun (x,y) -> clock_rec x, clock_rec y) up in
       { ce_desc = CImperative_update(e',up); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Variable n ->
      (* get the clock scheme of n in the env *)
      let sigma =
        begin
          try Clocking_ocl.clock_lookup n gamma
          with Not_found ->
            Error.print_error e.e_loc ("Unbound variable "^n)
        end
        (* instantiate it *)
      in
      let gs = Clocking_ocl.gen_instance sigma in
      { ce_desc = CVariable n; ce_loc = e.e_loc; ce_clock = gs }
    | InfixOp (op,e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocking_ocl.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CInfixOp(op,e1,e2) ; ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | Alternative (e1,e2,e3) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      let e3 = clock_rec e3 in
      Clocking_ocl.unify(e1.ce_clock,e2.ce_clock);
      Clocking_ocl.unify(e2.ce_clock,e3.ce_clock);
      { ce_desc = CAlternative(e1,e2,e3); ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | Application(id,e') ->
       begin
       try
         let fun_ck = Clocking_ocl.clock_lookup id !Clocking_ocl.global_typing_env in
         let fun_ck = Clocking_ocl.gen_instance fun_ck in
         let e' = clock_rec e' in
         let u = Var (Clocking_ocl.new_varclock ()) in
         let t = Arrow (e'.ce_clock,u) in
         Clocking_ocl.unify(t,fun_ck);
         { ce_desc = CApplication(id,e'); ce_loc = e.e_loc; ce_clock = u}
       with Not_found ->
             Error.print_error e.e_loc "Node not found"
       end
    | When (e',c) ->
      let s = Clocking_ocl.get_ident c in
      let e' = clock_rec e' in
      let c = clock_rec c in
      (* Clock of 'when' is 'a -> (c :: 'a) -> 'a on c *)
      let a = Var (Clocking_ocl.new_varclock ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),On(a,s)))) in
      (* clock of result *)
      let u = Var (Clocking_ocl.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      Clocking_ocl.unify(tt,new_type);
      { ce_desc = CWhen(e',c); ce_loc = e.e_loc; ce_clock = u}
    | Whennot (e',c) ->
      let s = Clocking_ocl.get_ident c in
      let e' = clock_rec e' in
      let c = clock_rec c in
      (* Clock of 'whenot' is 'a -> (c :: 'a) -> 'a on (not c) *)
      let a = Var (Clocking_ocl.new_varclock ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),Onnot(a,s)))) in
      (* clock of result *)
      let u = Var (Clocking_ocl.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      Clocking_ocl.unify(tt,new_type);
      { ce_desc = CWhennot(e',c); ce_loc = e.e_loc; ce_clock = u}
    | Merge (c,e1,e2) ->
      let s = Clocking_ocl.get_ident c in
      let c = clock_rec c in
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      (* Clock of 'whenot' is clk:'a -> 'a on c -> 'a on (not c) -> 'a *)
      let a = Var(Clocking_ocl.new_varclock () ) in
      let tt = Arrow(Carrier(s,a), Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
      let u = Var (Clocking_ocl.new_varclock () ) in
      let new_type = Arrow(c.ce_clock,Arrow(e1.ce_clock,Arrow(e2.ce_clock,u))) in
      Clocking_ocl.unify(tt,new_type);
      { ce_desc = CMerge(c,e1,e2); ce_loc = e.e_loc; ce_clock = u}
    | ETuple es ->
       let es = List.map clock_rec es in
       let clk = List.map (fun x -> x.ce_clock) es in
      { ce_desc = CETuple es; ce_loc = e.e_loc; ce_clock = CTuple clk }
    | Fby (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocking_ocl.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CFby(e1,e2); ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | PrefixOp (op,e') ->
      let e' = clock_rec e' in
      { ce_desc = CPrefixOp(op,e'); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Call e' ->
      let a = Var(Clocking_ocl.new_varclock ()) in
      { ce_desc = CCall e'; ce_loc = e.e_loc; ce_clock = a}
    | Pre _ | Arrow _ -> failwith "shouldn't happen"
  in
  clock_rec e

  (* let sigma = clock_e gamma e in *)
  (* { ce_desc = e.e_desc; *)
    (* ce_loc = e.e_loc; *)
    (* ce_clock = sigma} *)

let clock_equation gamma {pattern; expression} =
  let expression = clock_expr gamma expression
  in { cpattern = pattern; cexpression = expression}

let clock_equations gamma eqs =
  let aux eqs gamma x =
    let e = clock_equation gamma x in
    let clk = e.cexpression.ce_clock in
    (e::eqs,(e.cpattern,Clocking_ocl.generalise_type gamma clk)::gamma)
  in
  List.fold_left (fun (eqs,gamma) x -> aux eqs gamma x) ([],gamma) eqs

let clock_node gamma node =
  let open Clocking_ocl in
  let vars = get_all_vars node in
  let vars_clocks =  List.map (fun x -> (x,generalise_type gamma (Var(new_varclock())))) vars in
  let env = vars_clocks@gamma in
  let (eqs,gamma) = clock_equations env node.equations in
  let ckins = List.map (fun x -> lookup_clock env x) (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x) (split_tuple node.outputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_clock = generalise_type gamma (Arrow(ckins,ckouts)) in
  {
    cnode_clock = node_clock;
    cname = node.name;
    cinputs = node.inputs;
    coutputs = node.outputs;
    cequations = eqs }