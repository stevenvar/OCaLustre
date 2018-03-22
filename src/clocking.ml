open Parsing_ast
open Clocking_ast
open Carriers

let put_carrier (s,c) =
  match c.ce_clock with
  | Var k -> k.value <- Carrier(s,c.ce_clock)
  | _ -> ()

let clock_expr gamma (e:expression) =
  let rec clock_rec e =
    match e.e_desc with
    | Value v ->
        let clk = Var (Clocks.new_varclock ()) in
       { ce_desc = CValue v ; ce_loc = e.e_loc ; ce_clock = clk}
    | Unit ->
       let clk = Var (Clocks.new_varclock ()) in
       { ce_desc = CUnit ; ce_loc = e.e_loc ; ce_clock = clk}
    | Array e' ->
      let clk = Clocks.(new_varclock ()) in
      let clk = Var clk in
        let e' = List.map clock_rec e' in
        let y = List.hd e' in
        List.iter (fun x -> Clocks.unify(x.ce_clock,y.ce_clock)) e';
       { ce_desc = CArray e' ; ce_loc = e.e_loc; ce_clock = clk}
    | Array_get (e',n) ->
       let e' =  clock_rec e' in
       let n =  clock_rec n in
       Clocks.unify(e'.ce_clock,n.ce_clock);
       { ce_desc = CArray_get (e',n) ; ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Array_fold (e',f,acc) ->
      let e' = clock_rec e' in
      let acc = clock_rec acc in
      Clocks.unify(e'.ce_clock,acc.ce_clock);
      { ce_desc = CArray_fold(e',f,acc); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Array_map (e',f) ->
       let e' = clock_rec e' in
      { ce_desc = CArray_map(e',f); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Imperative_update (e',up) ->
       let e'= clock_rec e' in
       let up = List.map (fun (x,y) ->
                    let cx = clock_rec x in
                    let cy = clock_rec y in
                    Clocks.unify(e'.ce_clock,cx.ce_clock);
                    Clocks.unify(cx.ce_clock,cy.ce_clock);
                  (cx,cy)) up in
       { ce_desc = CImperative_update(e',up); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Variable n ->
      (* get the clock scheme of n in the env *)
      let sigma =
        begin
          try Clocks.clock_lookup n gamma
          with Not_found ->
            Forall([],[],Var(Clocks.new_varclock()))
            (* Error.print_error e.e_loc ("Unbound variable "^n) *)
        end
        (* instantiate it *)
      in
      let gs = Clocks.gen_instance sigma in
      { ce_desc = CVariable n; ce_loc = e.e_loc; ce_clock = gs }
    | InfixOp (op,e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CInfixOp(op,e1,e2) ; ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | Alternative (e1,e2,e3) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      let e3 = clock_rec e3 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      Clocks.unify(e2.ce_clock,e3.ce_clock);
      { ce_desc = CAlternative(e1,e2,e3); ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | Application(id,e') ->
       begin
       try
         let fun_ck = Clocks.clock_lookup id gamma in
         let fun_ck = Clocks.gen_instance fun_ck in
         let e' = clock_rec e' in
         let u = Var (Clocks.new_varclock ()) in
         let t = Arrow (e'.ce_clock,u) in
         Clocks.unify_with_carriers(t,fun_ck);
         { ce_desc = CApplication(id,e'); ce_loc = e.e_loc; ce_clock = u}
       with Not_found ->
         let s = Format.asprintf "Clock of node not found : %s" id in
         Error.print_error e.e_loc s
       end
    | When (e',c) ->
      let e' = clock_rec e' in
      let c = clock_rec c in
      let a = Var (Clocks.new_varclock ()) in
      let s = VarCar (Carriers.new_varcar ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),On(a,s)))) in
      (* clock of result *)
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      (* put_carrier(s,c); *)
      Clocks.unify(tt,new_type);
      { ce_desc = CWhen(e',c); ce_loc = e.e_loc; ce_clock = u}
    | Whennot (e',c) ->
      let s = VarCar (Carriers.new_varcar ()) in
      let e' = clock_rec e' in
      let c = clock_rec c in
      (* Clock of 'whenot' is 'a -> (c :: 'a) -> 'a on (not c) *)
      let a = Var (Clocks.new_varclock ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),Onnot(a,s)))) in
      (* clock of result *)
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      Clocks.unify(tt,new_type);
      { ce_desc = CWhennot(e',c); ce_loc = e.e_loc; ce_clock = u}
    | Merge (c,e1,e2) ->
      let s = VarCar (Carriers.new_varcar ()) in
      let c = clock_rec c in
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      (* Clock of 'whenot' is clk:'a -> 'a on c -> 'a on (not c) -> 'a *)
      let a = Var(Clocks.new_varclock () ) in
      let tt = Arrow(Carrier(s,a), Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
      let u = Var (Clocks.new_varclock () ) in
      let new_type = Arrow(c.ce_clock,Arrow(e1.ce_clock,Arrow(e2.ce_clock,u))) in
      Clocks.unify(tt,new_type);
      { ce_desc = CMerge(c,e1,e2); ce_loc = e.e_loc; ce_clock = u}
    | ETuple es ->
       let es = List.map clock_rec es in
       let clk = List.map (fun x -> x.ce_clock) es in
      { ce_desc = CETuple es; ce_loc = e.e_loc; ce_clock = CTuple clk }
    | Fby (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CFby(e1,e2); ce_loc = e.e_loc; ce_clock = e1.ce_clock}
    | PrefixOp (op,e') ->
      let e' = clock_rec e' in
      { ce_desc = CPrefixOp(op,e'); ce_loc = e.e_loc; ce_clock = e'.ce_clock}
    | Call e' ->
      let a = Var(Clocks.new_varclock ()) in
      { ce_desc = CCall e'; ce_loc = e.e_loc; ce_clock = a}
    | Pre _ | Arrow _ -> failwith "shouldn't happen"
  in
  try
    clock_rec e
  with Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock_scheme (Forall(vars1,[],c1))
                 print_clock_scheme (Forall(vars2,[],c2)) in
      Error.print_error e.e_loc s1
    end


let rec string_of_pat p =
  match p.p_desc with
  | Ident i -> i
  | Typed(p,t) -> string_of_pat p
  | PUnit -> "()"
  | _ -> failwith "sop"


let rec lookup env p =
   match p.p_desc with
  | Ident i ->
     (try
        List.assoc i env
      with Not_found ->
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
     let clocks = List.map (lookup env) t in
     let clocks = List.map Clocks.gen_instance clocks in
     let clk = CTuple clocks in
     Clocks.generalise_type env clk
  | PUnit -> failwith "unit"
  | Typed (p,t) -> lookup env p

let split_clock clock =
  match clock with
  | CTuple t -> t
  | _ -> [clock]

let split_pattern pat =
  match pat.p_desc with
  | Tuple p -> p
  | _ -> [pat]

let clock_equation gamma {pattern; expression} =
  let expression = clock_expr gamma expression in
  (* let pck = lookup gamma pattern in *)
  let open Clocks in
  (* need to unify clock of pattern and expression *)
  (* unify(gen_instance pck,expression.ce_clock); *)
  let names = split_pattern pattern in
  let names = List.map string_of_pat names in 
  let clocks = split_clock (Clocks.shorten(expression.ce_clock)) in
  let new_elem = List.map2 (fun x y -> (x,Forall([],[],y))) names clocks in
  (new_elem@gamma),{ cpattern = pattern; cexpression = expression}

let clock_equations gamma eqs =
  (* List.map (clock_equation gamma) eqs *)
  List.fold_left (fun (env,eqs) eq ->
    let (new_env,e) = clock_equation (List.rev env) eq in (new_env,e::eqs)) (gamma,[]) eqs

        
let remove_types p =
  match p.p_desc with
  | Typed(p,s) -> p
  | _ -> p

let split_tuple p =
  match p.p_desc with
  | Tuple pl -> List.map remove_types pl
  | _ -> [remove_types p]

let group_tuple pl =
  match pl with
  | [] -> Unknown
  | [x] -> x
  | _ -> CTuple pl



let get_all_vars node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  (* let vars = List.flatten vars in *)
  let vars = [] in
  let ins = split_tuple node.inputs in
  (* let outs = split_tuple node.outputs in *)
  Clocks.make_set (vars@ins)

let get_all_carriers node =
  let extract_carrier exp l =
    match exp.e_desc with
    | When(e1,e2) -> e2::l
    | _ -> l
  in
  let eqs = node.equations in
  let exps = List.map (fun x -> x.expression) eqs in
  let carriers = List.fold_left (fun acc x -> extract_carrier x acc) [] exps in
  carriers

let get_ident e =
  match e.e_desc with
  | Variable x -> x
  | _ -> failwith "get_ident"

let clock_node gamma node =
  let open Clocks in
  reset_varclocks ();
  reset_varcar ();
  let vars = get_all_vars node in
  let vars = List.map string_of_pat vars in 
  (* let vars = List.map string_of_pat vars in *)
  let cars = get_all_carriers node in
  let cars = List.map get_ident cars in
  List.iter print_string cars;
  let cars_clocks = List.map (fun x -> (x,Forall([],[],Carrier(VarCar(new_varcar()),Var(new_varclock()))))) cars in
  let vars_clocks =  List.map (fun x -> (x,Forall([],[],Var(new_varclock())))) vars in
  
  let env = cars_clocks@vars_clocks@gamma in
  (* print_env Format.std_formatter env; *)
  (* Clocks.print_env Format.std_formatter env; *)
  let (env,eqs) = clock_equations env node.equations in
  let ckins = List.map (fun x -> lookup_clock env x) (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x) (split_tuple node.outputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_clock = generalise_type [] (Arrow(ckins,ckouts)) in
  let new_env = (string_of_pat node.name,node_clock)::gamma in
  (new_env,
  {
    cnode_clock = node_clock;
    cname = node.name;
    cinputs = node.inputs;
    coutputs = node.outputs;
    cequations = eqs })
