open Parsing_ast
open Clocking_ast
open Carriers

(* let cars = ref [] *)

let rec extract_conds c =
  let c = Clocks.shorten c in
  begin
    match c with
    | On (x,c) ->
      (true,string_of_carrier c)::(extract_conds x)
    | Onnot (x,c) ->
      (false,string_of_carrier c)::(extract_conds x)
    | _ -> []
  end


  (* let c = Clocks.shorten c in
   * let open Carriers in
   * cars := List.map (fun (x,y) -> car_shorten x, y) !cars;
   * let s = Format.asprintf "extract cond %a \n" Clocks.print_clock (c,[]) in
   * (\* try *\)
   *   begin
   *     match c with
   *     | On (x,c) ->
   *       begin try
   *           let clk =  List.assoc (car_shorten c) !cars in
   *           (\* (true,clk)::[] *\)
   *           (true,clk)::(extract_conds x)
   *         with _ ->
   *           (\* failwith "?"; *\)
   *           List.iter (fun (x,y) -> Format.printf "%a -> %s\n" Carriers.print_carrier x y) !cars;
   *           Format.printf "What is %a ?" Carriers.print_carrier (car_shorten c);
   *           extract_conds x
   *       end
   *     | Onnot (x,c) ->
   *       begin try
   *           let clk =  List.assoc (car_shorten c) !cars in
   *           (\* (false,clk)::[] *\)
   *           (false,List.assoc c !cars)::(extract_conds x)
   *         with _ ->
   *           (\* failwith "?"; *\)
   *           extract_conds x
   *       end
   *     | Carrier (x,c) -> extract_conds c (\* ??? *\)
   *     | _ -> []
   *   end *)
  (* with _ -> Format.printf "%s" s ; failwith s *)

let rec get_cond c1 c2 : ( (bool * string) list) =
  let open Clocks in
  let s = Format.asprintf "get_cond : %a and %a \n"
      Clocks.print_clock (c1,[])
      Clocks.print_clock (c2,[])
  in
  (* Format.printf "%s" s; *)
  match Clocks.shorten c1, Clocks.shorten c2 with
  | CTuple tl , CTuple pl ->
    (* let l = List.map2 (fun x y -> (x,y)) tl pl in *)
    (* List.fold_left (fun acc (x,y) -> get_cond x y @ acc) [] l *)
    get_cond (List.hd tl) (List.hd pl)
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


let rec right_clock c =
  match c with
  | Arrow(a,b) -> b
  | Var c -> left_clock c.value
  | _ -> failwith "left_clock"

let get_ident e =
  match e.e_desc with
  | Variable x -> x
  | _ -> failwith "get_ident"

(* let get_conds gamma eq =
 *   let rec aux ce =
 *     match ce.ce_desc with
 *     | CApplication(id,num,ce') ->
 *       begin try
 *           let fun_ck = Clocks.clock_lookup id gamma in
 *           let fun_ck' = Clocks.gen_instance fun_ck in
 *           let params = left_clock (fun_ck') in
 *           (\* let c = get_cond params ce'.ce_clock in *\)
 *           (\* match c with *\)
 *           (\* | [] -> ce.ce_desc *\)
 *           (\* | _ -> CCondact(c,ce) *\)
 *           ce
 *         with Not_found ->
 *           let s = Format.asprintf "Clock of node not found : %s" id in
 *           Error.print_error ce.ce_loc s
 *       end
 *     | _ -> ce.ce_desc
 *   in
 *   { eq with cexpression = { eq.cexpression with ce_desc = aux eq.cexpression}} *)

let get_conds gamma eq = eq

let clock_expr gamma (e:expression) =
  let rec clock_rec e =
  (* Format.printf "Clocking expression %a \n" *)
     (* Parsing_ast_printer.print_expression e; *)
    match e.e_desc with
    | Value Nil ->
      let clk = Var (Clocks.new_varclock ()) in clk
    | Value v ->
      let clk = Var (Clocks.new_varclock ()) in clk
    | Unit ->
       let clk = Var (Clocks.new_varclock ()) in clk
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
      gs
    | InfixOp (op,e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1,e2);
      e1
    | Alternative (e1,e2,e3) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      let e3 = clock_rec e3 in
      Clocks.unify(e1,e2);
      Clocks.unify(e2,e3);
      e1
    | Application(id,num,e') ->
      let fun_ck = Clocks.clock_lookup id gamma in
      let fun_ck' = Clocks.gen_instance fun_ck in
      let ce' = clock_rec e' in
      let get_one_input_clock =
        function CTuple tl -> List.hd tl
               | c -> c
      in
      let c = get_one_input_clock ce' in
      let clk = right_clock fun_ck' in
      let rec put_all_output_clocks c clk =
        match clk with
          CTuple tl -> CTuple (List.map (fun _ -> c) tl)
        | k -> c
      in
      let cc = put_all_output_clocks c clk in
      cc
    (* let's just say that the outputs is on the clock of the inputs *)
    (*   let get_one_input_clock =
         *     function CTuple tl -> List.hd tl
         *            | c -> c
         *   in
         *   let c = get_one_input_clock ce' in
         *   let u = Var (Clocks.new_varclock ()) in
         *   (\* let t = Arrow (ce',u) in *\)
         *   (\* Clocks.unify(t,fun_ck'); *\)
         *   let rec put_all_output_clocks c clk =
         *     let rec aux l =
         *       match l with
         *         [] -> []
         *       | h::t -> c::(aux t)
         *     in
         *     match clk with
         *       CTuple tl -> CTuple (aux tl)
         *     | c -> c
         *   in
         *   let u = put_all_output_clocks c u in
         *   u
         * with Not_found ->
         *   let s = Format.asprintf "Clock of node not found : %s" id in
         *   Error.print_error e.e_loc s *)

    | Clock e -> clock_rec e
    | When (e',c) ->
      let e' = clock_rec e' in
      let c' = clock_rec c in
      let a = Var (Clocks.new_varclock ()) in
      let s = NameCar (get_ident c) in
      let tt = (Arrow (a,Arrow(a,On(a,s)))) in
      (* clock of result *)
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e',Arrow(c',u)) in
      Clocks.unify(tt,new_type);
      u
    | Whennot (e',c) ->
      let e' = clock_rec e' in
      let c' = clock_rec c in
      let s = NameCar (get_ident c) in
      let a = Var (Clocks.new_varclock ()) in
      let tt = (Arrow (a,Arrow(a,Onnot(a,s)))) in
      let u = Var (Clocks.new_varclock ()) in
      let new_type = Arrow(e',Arrow(c',u)) in
      (* Clocks.unify_with_carriers(new_type,tt); *)
      Clocks.unify(tt,new_type);
      u
    | Merge (c,e1,e2) ->
      let c' = clock_rec c in
      let s = NameCar (get_ident c) in
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      let a = Var(Clocks.new_varclock () ) in
      let tt = Arrow(a, Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
      let u = Var (Clocks.new_varclock () ) in
      let new_type = Arrow(c',Arrow(e1,Arrow(e2,u))) in
      Clocks.unify(tt,new_type);
      u
    | ETuple es ->
      let es = List.map clock_rec es in
      (* say that the clock of a tuple should be the same everywhere *)
      let c = Var (Clocks.new_varclock ()) in
      List.iter (fun x -> Clocks.unify(x,c)) es;
      (* CTuple es *)
      c
    | Fby (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1,e2);
      e1
    | PrefixOp (op,e') ->
      let e' = clock_rec e' in
      e'
    | Call e' ->
      let a = Var(Clocks.new_varclock ()) in
      a
    | Pre e ->
      let e' = clock_rec e in
      e'
    | Arrow (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1,e2);
      e1
    | Array el ->
      let clks = List.map clock_rec el in
      let c = List.hd clks in
      List.iter (fun x -> Clocks.unify(c,x)) clks;
      List.hd clks
    | Imperative_update (e,l) ->
      clock_rec e
    | Array_get (e1,e2) ->
      let e1 = clock_rec e1 in
      let e2 = clock_rec e2 in
      Clocks.unify(e1,e2);
      e1
    | _ -> failwith "todo"
  in
  try
    let cd = clock_rec e in
    (* Format.printf "==> %a\n" Clocks.print_clock (cd.ce_clock,[]); *)
    cd
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
     (* let clocks = List.map Clocks.gen_instance clocks in *)
     let clk = CTuple clocks in clk
     (* Clocks.generalise_type env clk *)
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
  (* Clocks.print_env Format.std_formatter gamma; *)
  let clock = clock_expr gamma expression in
  let gamma' = List.map (fun (x,y) -> x, Clocks.gen_instance y) gamma in
  let p = match pattern.p_desc with Tuple tl -> List.hd tl | p -> pattern in
  let pck = lookup gamma' p in
  let open Clocks in
  try
   ( match pattern.p_desc with
    | Tuple tl -> List.iter (fun x -> unify(lookup gamma' x,clock)) tl
    | p -> unify(lookup gamma' pattern,clock)
   )  ;
    (* ((string_of_pat pattern,generalise_type gamma pck)::gamma) *)
  gamma,{ cpattern = pattern; cexpression = expression ; cclock = clock }
  with Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock (c1,vars1)
                 print_clock (c2,vars2) in
      Error.print_error expression.e_loc s1;
    end
      | Carriers.CarrierClash (c1,c2) ->
        begin
          let open Carriers in
          let s1 = Format.asprintf
              "Carrier clash between %a and %a"
              print_carrier c1
              print_carrier c2 in
          Error.print_error expression.e_loc s1
        end

let unify_error (c1,c2) loc =
  try
    Clocks.unify (c1,c2);
  with Clocks.ClockClash (c1,c2) ->
    begin
      let open Clocks in
      let vars1 = (vars_of_clock c1) in
      let vars2 = (vars_of_clock c2) in
      let s1 = Format.asprintf
                 "Clock clash between %a and %a"
                 print_clock (c1,vars1)
                 print_clock (c2,vars2) in
      Error.print_error loc s1;
    end
      | Carriers.CarrierClash (c1,c2) ->
        begin
          let open Carriers in
          let s1 = Format.asprintf
              "Carrier clash between %a and %a"
              print_carrier c1
              print_carrier c2 in
          Error.print_error loc s1
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
  let env = vars_clocks@gamma in
  (* print_env Format.std_formatter env; *)
  (* Clocks.print_env Format.std_formatter env; *)
  let (env,eqs) = clock_equations env node.equations in
  (* print_env Format.std_formatter env; *)
  let eqs = List.map (get_conds env) eqs in
  let ckins = List.map (fun x -> lookup_clock env x)
      (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x)
      (split_tuple node.outputs) in
  let ckins = List.map gen_instance ckins in
  List.iter (fun c -> unify_error(c,Base) node.inputs.p_loc) ckins;
  let ckins = group_tuple ckins in
  let ckouts = List.map gen_instance ckouts in
  List.iter (fun c -> unify_error(c,Base) node.outputs.p_loc) ckouts;
  let ckouts = group_tuple ckouts in
  let node_clock = Arrow(ckins,ckouts) in
  (* let node_clock = Clocks.full_shorten node_clock in *)
  (* Format.printf "after shortening %a \n" print_clock (node_clock,[]); *)
  let node_clock = generalise_type [] node_clock in
  (* Format.printf "after generalizing %a \n" print_clock_scheme node_clock; *)
  let new_env = (string_of_pat node.name,node_clock)::gamma in
  (new_env,
  {
    cnode_clock = node_clock;
    cname = node.name;
    cinputs = node.inputs;
    coutputs = node.outputs;
    cequations = List.rev eqs })
