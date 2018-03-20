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
       let e' =  clock_rec e' in
       let n =  clock_rec n in
       Clocking_ocl.unify(e'.ce_clock,n.ce_clock);
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
       let e'= clock_rec e' in
       let up = List.map (fun (x,y) ->
                    let cx = clock_rec x in
                    let cy = clock_rec y in
                    Clocking_ocl.unify(e'.ce_clock,cx.ce_clock);
                    Clocking_ocl.unify(cx.ce_clock,cy.ce_clock);
                  (cx,cy)) up in
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
         let fun_ck = Clocking_ocl.clock_lookup id gamma in
         let fun_ck = Clocking_ocl.gen_instance fun_ck in
         let e' = clock_rec e' in
         let u = Var (Clocking_ocl.new_varclock ()) in
         let t = Arrow (e'.ce_clock,u) in
         Clocking_ocl.unify(t,fun_ck);
         { ce_desc = CApplication(id,e'); ce_loc = e.e_loc; ce_clock = u}
       with Not_found ->
         let s = Format.asprintf "Clock of node not found : %s" id in
         Error.print_error e.e_loc s
       end
    | When (e',c) ->
      let s = Clocking_ocl.get_ident c in
      let e' = clock_rec e' in
      let c =  clock_rec c in
      (* Clocking_ocl.unify(e'.ce_clock,c.ce_clock); *)
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
      let tt = (Arrow (a,Arrow(a,Onnot(a,s)))) in
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
      let tt = Arrow(a, Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
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
  try
    clock_rec e
  with Clocking_ocl.ClockClash (c1,c2) ->
    begin
      let open Clocking_ocl in
      let vars = (vars_of_clock c1)@(vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock_scheme (Forall(vars,c1))
                 print_clock_scheme (Forall(vars,c2)) in
      Error.print_error e.e_loc s1
    end

let clock_equation gamma {pattern; expression} =
  let expression = clock_expr gamma expression in
  let pck = Clocking_ocl.lookup_clock gamma pattern in
  let open Clocking_ocl in
  (* need to unify clock of pattern and expression *)
  unify(gen_instance pck,expression.ce_clock);
  { cpattern = pattern; cexpression = expression}

let clock_equations gamma eqs =
  List.map (clock_equation gamma) eqs

let clock_node gamma node =
  let open Clocking_ocl in
  reset_varclocks () ;
  let vars = get_all_vars node in
  let vars_clocks =  List.map (fun x -> (x,Forall([],Var(new_varclock())))) vars in
  let env = vars_clocks@gamma in
  (* print_env Format.std_formatter env; *)
  (* Clocking_ocl.print_env Format.std_formatter env; *)
  let eqs = clock_equations env node.equations in
  let ckins = List.map (fun x -> lookup_clock env x) (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x) (split_tuple node.outputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_clock = generalise_type [] (Arrow(ckins,ckouts)) in
  let new_env = (node.name,node_clock)::gamma in
  (new_env,
  {
    cnode_clock = node_clock;
    cname = node.name;
    cinputs = node.inputs;
    coutputs = node.outputs;
    cequations = eqs })
