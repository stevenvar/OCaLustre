open Parsing_ast
open Clocking_ast
open Carriers


let rec extract_conds c =
  let c = Clocks.shorten c in
  let open Carriers in
  (* Format.printf "extract cond %a" Clocks.print_clock (c,[]); *)
  match c with
  | On (x,c) -> (true,string_of_carrier c)::(extract_conds x)
  | Onnot (x,c) -> (false,string_of_carrier c)::(extract_conds x)
  | Carrier (x,c) -> extract_conds c
  | _ -> []

let rec get_cond c1 c2 : ( (bool * string) list) =
  let open Clocks in
  let s = Format.asprintf "get_cond : %a and %a \n"
      Clocks.print_clock (c1,[])
      Clocks.print_clock (c2,[])
  in
  (* Format.printf "%s" s; *)
  match Clocks.shorten c1, Clocks.shorten c2 with
  | CTuple tl , CTuple pl ->
    let l = List.map2 (fun x y -> (x,y)) tl pl in
    List.fold_left (fun acc (x,y) -> get_cond x y @ acc) [] l
  | Var a, Var b -> []
  | On (a,c), On (b,d) -> get_cond a b
  | Onnot (a,c), Onnot (b,d) -> get_cond a b
  | Carrier(s,c), Carrier(t,d) -> get_cond c d
  | Var a, k -> extract_conds k
  | _ ->
    failwith s

let rec left_clock c =
  match c with
  | Arrow(a,b) -> a
  | Var c -> left_clock c.value
  | _ -> failwith "left_clock"


let get_ident e =
  match e.ce_desc with
  | CVariable x -> x
  | _ -> failwith "get_ident"

let get_conds gamma eq =
  let rec aux ce =
    match ce.ce_desc with
    | CApplication(id,num,ce') ->
      begin try
          let fun_ck = Clocks.clock_lookup id gamma in
          let fun_ck' = Clocks.gen_instance fun_ck in
          let params = left_clock (fun_ck') in
          let c = get_cond params ce'.ce_clock in
          (* Format.printf "Conditions for %s (%a) \n" id *)
            (* Clocks.print_clock (ce'.ce_clock, []); *)
          (* List.iter (fun (x,y) -> Format.printf "%s = %b \n" y x) c; *)
          match c with
          | [] -> ce.ce_desc
          | _ -> CCondact(c,ce)
        with Not_found ->
          let s = Format.asprintf "Clock of node not found : %s" id in
          Error.print_error ce.ce_loc s
      end
    | _ -> ce.ce_desc
  in
  { eq with cexpression = { eq.cexpression with ce_desc = aux eq.cexpression}}

let clock_expr gamma (e:expression) =
  (* Format.printf "Clocking expression %a \n"
     Parsing_ast_printer.print_expression e; *)
  let rec clock_rec e =
    match e.e_desc with
    | Value v ->
        let clk = Var (Clocks.new_varclock ()) in
        { ce_desc = CValue v ;
          ce_loc = e.e_loc ;
          ce_clock = clk }
    | Unit ->
       let clk = Var (Clocks.new_varclock ()) in
       { ce_desc = CUnit ;
         ce_loc = e.e_loc ;
         ce_clock = clk }
    | Array e' ->
      let clk = Clocks.(new_varclock ()) in
      let clk = Var clk in
        let e' = List.map clock_rec e' in
        let y = List.hd e' in
        List.iter (fun x -> Clocks.unify(x.ce_clock,y.ce_clock)) e';
        { ce_desc = CArray e' ;
          ce_loc = e.e_loc;
          ce_clock = clk }
    | Array_get (e',n) ->
       let e' =  clock_rec e' in
       let n =  clock_rec n in
       Clocks.unify(e'.ce_clock,n.ce_clock);
       { ce_desc = CArray_get (e',n) ;
         ce_loc = e.e_loc;
         ce_clock = e'.ce_clock }
    | Array_fold (e',f,acc) ->
      let e' = clock_rec e' in
      let acc = clock_rec acc in
      Clocks.unify(e'.ce_clock,acc.ce_clock);
      { ce_desc = CArray_fold(e',f,acc);
        ce_loc = e.e_loc;
        ce_clock = e'.ce_clock }
    | Array_map (e',f) ->
       let e' = clock_rec e' in
       { ce_desc = CArray_map(e',f);
         ce_loc = e.e_loc;
         ce_clock = e'.ce_clock }
    | Imperative_update (e',up) ->
       let e'= clock_rec e' in
       let up = List.map (fun (x,y) ->
                    let cx = clock_rec x in
                    let cy = clock_rec y in
                    Clocks.unify(e'.ce_clock,cx.ce_clock);
                    Clocks.unify(cx.ce_clock,cy.ce_clock);
                  (cx,cy)) up in
       { ce_desc = CImperative_update(e',up);
         ce_loc = e.e_loc;
         ce_clock = e'.ce_clock }
    | Variable n ->
      (* get the clock scheme of n in the env *)
      let sigma =
        begin
          try Clocks.clock_lookup n gamma
          with Not_found ->
            (* Forall([],[],Var(Clocks.new_varclock())) *)
            Error.print_error e.e_loc ("Unbound variable "^n)
        end
        (* instantiate it *)
      in
      let gs = Clocks.gen_instance sigma in
      { ce_desc = CVariable n;
        ce_loc = e.e_loc;
        ce_clock = gs ;
        }
    | InfixOp (op,e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CInfixOp(op,e1,e2) ;
        ce_loc = e.e_loc;
        ce_clock = e1.ce_clock }
    | Alternative (e1,e2,e3) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      let e3 = clock_rec e3 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      Clocks.unify(e2.ce_clock,e3.ce_clock);
      { ce_desc = CAlternative(e1,e2,e3);
        ce_loc = e.e_loc;
        ce_clock = e1.ce_clock }
    | Application(id,num,e') ->
      begin
        try
          (* Format.printf "ENV = %a\n" Clocks.print_env gamma; *)
          let fun_ck = Clocks.clock_lookup id gamma in
          (* Format.printf "THE CLOCK = %a\n"
             Clocks.print_clock_scheme fun_ck; *)
          let fun_ck' = Clocks.gen_instance fun_ck in
          let e' = clock_rec e' in
          let u = Var (Clocks.new_varclock ()) in
          let t = Arrow (e'.ce_clock,u) in
          Clocks.unify_with_carriers(t,fun_ck');
          (* let params = left_clock (Clocks.gen_instance fun_ck) in *)
          (* let c = get_cond params e'.ce_clock in *)
          (* Format.printf "Conditions for %s (%a) \n" id *)
            (* Clocks.print_clock (e'.ce_clock, []); *)
          (* List.iter (fun (x,y) -> Format.printf "%s = %b \n" y x) c; *)
          let exp = { ce_desc = CApplication(id,num,e');
                      ce_loc = e.e_loc;
                      ce_clock = u } in
          exp
        with Not_found ->
          let s = Format.asprintf "Clock of node not found : %s" id in
          Error.print_error e.e_loc s
      end
    | When (e',c) ->
      let e' = clock_rec e' in
      let c = clock_rec c in
      let a = Var (Clocks.new_varclock ()) in
      let s = NameCar (get_ident c) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),On(a,s)))) in
      (* clock of result *)
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      Clocks.unify_with_carriers(new_type,tt);
      { ce_desc = CWhen(e',c);
        ce_loc = e.e_loc;
        ce_clock = u }
    | Whennot (e',c) ->
      let e' = clock_rec e' in
      let c = clock_rec c in
      let s = NameCar (get_ident c) in
      (* Clock of 'whenot' is 'a -> (c : 'a) -> 'a on (not c) *)
      let a = Var (Clocks.new_varclock ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),Onnot(a,s)))) in
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e'.ce_clock,Arrow(c.ce_clock,u)) in
      Clocks.unify_with_carriers(new_type,tt);
      { ce_desc = CWhennot(e',c);
        ce_loc = e.e_loc;
        ce_clock = u }
    | Merge (c,e1,e2) ->
      let c = clock_rec c in
      let s = NameCar (get_ident c) in
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      (* Clock of 'merge' is (c:'a) -> 'a on c -> 'a on (not c) -> 'a *)
      let a = Var(Clocks.new_varclock () ) in
      let tt = Arrow(Carrier(s,a), Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
      let u = Var (Clocks.new_varclock () ) in
      let new_type = Arrow(c.ce_clock,Arrow(e1.ce_clock,Arrow(e2.ce_clock,u))) in
      Clocks.unify_with_carriers(tt,new_type);
      { ce_desc = CMerge(c,e1,e2);
        ce_loc = e.e_loc;
        ce_clock = u }
    | ETuple es ->
      let es = List.map clock_rec es in
      let clk = List.map (fun x -> x.ce_clock) es in
      { ce_desc = CETuple es;
        ce_loc = e.e_loc;
        ce_clock = CTuple clk }
    | Fby (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CFby(e1,e2);
        ce_loc = e.e_loc;
        ce_clock = e1.ce_clock }
    | PrefixOp (op,e') ->
      let e' = clock_rec e' in
      { ce_desc = CPrefixOp(op,e');
        ce_loc = e.e_loc;
        ce_clock = e'.ce_clock }
    | Call e' ->
      let a = Var(Clocks.new_varclock ()) in
      { ce_desc = CCall e';
        ce_loc = e.e_loc;
        ce_clock = a;
        }
    | Pre e ->
      let e' = clock_rec e in
      { ce_desc = CPre e';
        ce_loc = e.e_loc;
        ce_clock = e'.ce_clock }
    | Arrow (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1.ce_clock,e2.ce_clock);
      { ce_desc = CArrow (e1,e2) ;
        ce_loc = e.e_loc;
        ce_clock = e1.ce_clock }
  in
  try
    clock_rec e
  with Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clocks clash between %a and %a"
                 print_clock (c1,vars1)
                 print_clock (c2,vars2) in
      Error.print_error e.e_loc s1
    end
      | Carriers.CarrierClash (c1,c2) ->
        begin
          let open Carriers in
          let s1 = Format.asprintf
              "Carrier clash between %a and %a"
              print_carrier c1
              print_carrier c2 in
          Error.print_error e.e_loc s1
        end

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
  try
    (* unify_with_carriers (gen_instance pck, expression.ce_clock); *)
    (gamma),{ cpattern = pattern; cexpression = expression}
  with Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock (c1,vars1)
                 print_clock (c2,vars2) in
      Error.print_error expression.ce_loc s1;
    end
      | Carriers.CarrierClash (c1,c2) ->
        begin
          let open Carriers in
          let s1 = Format.asprintf
              "Carrier clash between %a and %a"
              print_carrier c1
              print_carrier c2 in
          Error.print_error expression.ce_loc s1
        end

let maj_clocks gamma {cpattern; cexpression} =
  let pck = lookup gamma cpattern in
  let open Clocks in
  try
    unify(gen_instance pck, cexpression.ce_clock);
  with  Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock (c1,vars1)
                 print_clock (c2,vars2) in
      Error.print_error cexpression.ce_loc s1
    end
      | Carriers.CarrierClash (c1,c2) ->
        begin
          let open Carriers in
          let s1 = Format.asprintf
              "Carrier clash between %a and %a"
              print_carrier c1
              print_carrier c2 in
          Error.print_error cexpression.ce_loc s1
        end


let clock_equations gamma eqs =
  List.fold_left (fun (env,eqs) eq ->
      let (new_env,e) = clock_equation (List.rev env) eq in
      (new_env,e::eqs)) (gamma,[]) eqs


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
  let vars = List.flatten vars in
  (* let vars = [] in *)
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

let clock_node gamma node =
  let open Clocks in
  reset_varclocks ();
  reset_varcar ();
  let vars = get_all_vars node in
  let vars = List.map string_of_pat vars in
  (* let vars = List.map string_of_pat vars in *)
  (* let cars = get_all_carriers node in *)
  (* let cars = List.map get_ident cars in *)
  (* let cars_clocks = List.map
     (fun x -> (x,Forall([],[],
     Carrier(VarCar(new_varcar()),Var(new_varclock()))))) cars in *)
  let vars_clocks =
    List.map (fun x -> (x,Forall([],[],Var(new_varclock())))) vars in
  (* let env = cars_clocks@vars_clocks@gamma in *)
  let env = vars_clocks@gamma in
  (* print_env Format.std_formatter env; *)
  (* Clocks.print_env Format.std_formatter env; *)
  let (env,eqs) = clock_equations env node.equations in
  List.iter (maj_clocks env) eqs;
  let eqs = List.map (get_conds env) eqs in
  let ckins = List.map (fun x -> lookup_clock env x)
      (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x)
      (split_tuple node.outputs) in
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
    cequations = List.rev eqs })
