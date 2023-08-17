open Clocking_ast
open Parsing_ast
open Clocking_ast_printer

let outputs = ref []

let split_ct ct =
  match ct with
  | Ck c -> [Ck c]
  | CkTuple cks -> cks

let group_ct l =
  match l with
  | [] -> failwith "empty list"
  | [Ck x] -> Ck x
  | _ -> CkTuple l

let rec carriers ck = match ck with
  | CkBase -> []
  | CkVariable { value = c; _} -> carriers c
  | Ckon (ck,s)
  | Ckonnot (ck,s) -> s::(carriers ck)
  | _ -> failwith "carriers"

let rec carriers_list ct = match ct with
  | Ck ck -> carriers ck
  | CkTuple (Ck c::cks) -> (carriers c)@(carriers_list (CkTuple cks))
  | CkTuple [] -> []
  | _ -> failwith "carriers_list "

let rec subst_base ck ck' =
  match ck with
  | CkUnknown -> ck
  (* failwith "subst_base : unknown" *)
  | CkBase -> ck'
  | CkVariable { value = c ; _} -> subst_base c ck'
  | Ckon (ck,s) -> Ckon(subst_base ck ck',s)
  | Ckonnot (ck,s) -> Ckonnot(subst_base ck ck',s)
(* | _ -> failwith "subst_base" *)

let rec subst_base_ct ct ck' =
  match ct with
  | Ck ck -> Ck (subst_base ck ck')
  | CkTuple cks -> CkTuple (List.map (fun c -> subst_base_ct c ck') cks)

let app_sigma sigma x =
  try List.assoc x sigma with _ -> x

let rec subst ck sigma =
  match ck with
  | CkBase -> CkBase
  | CkUnknown -> failwith "subst : unknown"
  | CkVariable { value = CkUnknown; _} -> ck
  | CkVariable { value = c ; _} -> subst c sigma
  | Ckon (ck,s) -> Ckon(subst ck sigma, app_sigma sigma s)
  | Ckonnot (ck,s) -> Ckonnot(subst ck sigma, app_sigma sigma s)

let rec subst_names cks sigma =
  match cks with
  | [] -> []
  | c::cks -> (subst c sigma)::(subst_names cks sigma)

let rec subst_ct ct sigma =
  match ct with
  | Ck ck -> Ck (subst ck sigma)
  | CkTuple (Ck c::cks) ->
    let cks = List.map (fun c -> subst_ct c sigma) cks in
    let c = subst c sigma in
    CkTuple (Ck c::cks)
  | CkTuple [] -> CkTuple []
  | _ -> failwith "subst_ct"

let subst_list sigma l =
  List.fold_left (fun acc x -> subst_ct x sigma ::acc) [] l

let rec get_subst xs es s =
  match xs,es with
  | [], [] -> []
  |  x::xs, { ce_desc = CVariable y ; _}::es ->
    if List.mem x s then
      let sigma = get_subst xs es s in
      (x,y)::sigma
    else get_subst xs es s
  |  x::xs, _::es ->
    if List.mem x s then (failwith "parameters that are clocks must be given a name") else
      get_subst xs es s
  | _, _ -> failwith "get_subst"

exception Incorrect_params

let rec get_subst_vars (xs:Parsing_ast.ident list) es s =
  match xs,es with
  | [], [] -> []
  |  x::xs, y::ys ->
    if List.mem x s then
      let sigma = get_subst_vars xs ys s in
      (x,y)::sigma
    else get_subst_vars xs ys s
  | _, _ ->
    raise Incorrect_params

(** Printing **)

(* Printing variables  *)
let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

(** Variables **)
let new_varclk, reset_varclk =
  let cpt = ref 0 in
  (fun () ->
     incr cpt;
     CkVariable { index = !cpt ; value = CkUnknown }),
  (fun () -> cpt := 0)

(** Shorten **)
exception Occurs

let occurs_ck { index = n; value = _} =
  let rec occrec = function
    | CkVariable { index = m; value = _} -> if n = m then raise Occurs
    | CkUnknown -> raise Occurs
    | CkBase -> ()
    | Ckon (ck,_x) -> occrec ck
    | Ckonnot (ck,_x) -> occrec ck
  in
  occrec

let rec occurs_ct var =
  function
  | Ck ck -> occurs_ck var ck
  | CkTuple cts -> List.iter (fun s -> occurs_ct var s) cts

let rec shorten_ck c =
  match c with
  | CkUnknown -> failwith "shorten"
  | CkVariable { index = _; value = CkUnknown} -> c
  | CkVariable { index = _;
                 value = CkVariable ({ index = _;
                                       value = CkUnknown }) as tv} -> tv
  | CkVariable ({ index = _; value = CkVariable tv1 } as tv2) ->
    tv2.value <- tv1.value;
    shorten_ck c
  | CkVariable { index = _ ; value = t' } -> shorten_ck t'
  | Ckon (ck,x) -> Ckon (shorten_ck ck, x)
  | Ckonnot (ck,x) -> Ckonnot (shorten_ck ck, x)
  | _ -> c

let rec shorten_ct c =
  match c with
  | Ck ck -> Ck (shorten_ck ck)
  | CkTuple cts -> CkTuple (List.map shorten_ct cts)

(** Unify **)
exception Unify_ck of ck * ck
exception Unify_ct of ct * ct * cexpression * cexpression * (Location.t option)

let rec unify_ck c1 c2 =
  let t1 = shorten_ck c1 in
  let t2 = shorten_ck c2 in
  (* Format.printf "Unify %a and %a \n" print_ck c1 print_ck c2; *)
  try
    (match t1, t2 with
     | CkBase, CkBase -> ()
     | CkVariable ({ index = n ; value = CkUnknown } as tv1),
       CkVariable ({ index = m; value = CkUnknown }) ->
       if n <> m then tv1.value <- t2
     | CkVariable ({ index = _n ; value = CkUnknown } as tv), c2 ->
       occurs_ck tv c2;
       tv.value <- c2
     | c1 , CkVariable ({ index = _n ; value = CkUnknown } as tv) ->
       occurs_ck tv c1;
       tv.value <- c1
     | Ckon(ck,s), Ckon(ck',s') ->
       if s = s' then unify_ck ck ck'
       else raise (Unify_ck (t1,t2))
     | Ckonnot(ck,s), Ckonnot(ck',s') ->
       if s = s' then unify_ck ck ck'
       else raise (Unify_ck (t1,t2))
     | _ -> raise (Unify_ck (t1, t2)))
  with Occurs -> raise (Unify_ck (t1,t2))

let unify_ct e1 e2 loc =
  let rec aux c1 c2 =
    let c1 = shorten_ct c1 in
    let c2 = shorten_ct c2 in
    (* Format.printf "Unify %a and %a \n" print_ct c1 print_ct c2; *)
    try
      (match c1,c2 with
       | Ck c1, Ck c2 -> unify_ck c1 c2
       | CkTuple cts, CkTuple cts' ->
         (try
            List.iter2 (fun c d -> aux c d) cts cts'
          with Invalid_argument _ -> Error.print_error Location.none "Not the same number of elements")
       | _ ->
         raise (Unify_ct (c1,c2,e1,e2,loc)))
    with Unify_ck (_,_) -> raise (Unify_ct (e1.ce_clk,e2.ce_clk,e1,e2,loc))
  in
  aux e1.ce_clk e2.ce_clk

(** Instantiation **)
let gen_instance (gv,tau) =
  let gv_unknowns = List.map (fun n -> n, new_varclk ()) gv in
  let rec ginstance t =
    match t with
    | CkVariable { index = n; value = CkUnknown}->
      begin try
          List.assoc n gv_unknowns
        with Not_found -> t
      end
    | CkVariable { index = _; value = t } -> ginstance t
    | CkUnknown -> failwith "gen_instance"
    | Ckon (ck,x) -> Ckon(ginstance ck, x)
    | Ckonnot (ck,x) -> Ckonnot(ginstance ck, x)
    | CkBase -> CkBase
  in
  ginstance tau

let rec gen_instance_ct (gv,tau) =
  match tau with
  | Ck ck -> Ck (gen_instance (gv,ck))
  | CkTuple cts -> CkTuple (List.map (fun ct -> gen_instance_ct (gv,ct)) cts)


let rec get_cond c1 c2 =
  match shorten_ck c1, shorten_ck c2 with
  | CkBase, c -> c
  | Ckon (a,_c), Ckon (b,_d) -> get_cond a b
  | Ckonnot (a,_c), Ckonnot (b,_d) -> get_cond a b
  | _ -> failwith "get_cond"


let rec get_cond_ct c1 c2 =
  match c1, c2 with
  | Ck c, Ck c' -> get_cond c c'
  | CkTuple cks, CkTuple cks' ->
    let c = List.hd cks in
    let d = List.hd cks' in
    get_cond_ct c d
  | _ -> failwith "get_cond_ct"


(** Generalization **)
let generalize_sign (cin,cout) =
  let v = new_varclk () in
  let generalize_ck c =
    let rec aux c =
      match c with
      | CkBase -> v
      | CkUnknown -> failwith "generalize"
      | Ckon (ck,x) -> Ckon(aux ck,x)
      | Ckonnot (ck,x) -> Ckonnot(aux ck,x)
      | CkVariable { index = n ; value = t } ->
        CkVariable {index = n; value = aux t}
    in
    aux c
  in
  let rec aux c =
    match c with
    | Ck ck -> Ck (generalize_ck ck)
    | CkTuple cts -> CkTuple (List.map aux cts)
  in
  (aux cin,aux cout)

(* returns clk vars *)
let vars_of_clk tau =
  let rec vars vs v =
    match v with
    | CkUnknown -> failwith "vars_of_clk"
    | CkVariable { index = n; value = CkUnknown} ->
      if List.mem n vs then vs else (n::vs)
    | CkVariable { index = _n; value = t } -> vars vs t
    | Ckon (ck,_x) -> vars vs ck
    | Ckonnot (ck,_x) -> vars vs ck
    | CkBase -> vs
  in vars [] tau

let vars_of_clk_ct tau =
  let rec vars vs v =
    match v with
    | Ck ck -> vars_of_clk ck
    | CkTuple cts -> List.fold_left (fun acc ct -> vars acc ct) vs cts
  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_clk bv t =
  let vs  = vars_of_clk_ct t in
  substract vs bv

let unknowns_of_clk_env env =
  let vs = List.fold_left (fun vs (gv,t) ->
      let v = unknowns_of_clk gv t in
      (v::vs)) [] env in
  List.flatten vs

let rec replace_vars_by_base tau =
  match tau with
  | CkVariable ({ index = _n; value = CkUnknown } as tv) ->
    tv.value <- CkBase
  | Ckon (ck,_x) -> replace_vars_by_base ck
  | Ckonnot (ck,_x) -> replace_vars_by_base ck
  | CkVariable { index = _n; value = t} ->
    replace_vars_by_base t
  | CkBase -> ()
  | CkUnknown -> failwith "unknown"

let rec replace_vars_by_base_ct tau =
  match tau with
  | Ck ck -> replace_vars_by_base ck
  | CkTuple cts -> List.iter replace_vars_by_base_ct cts

let generalize_clk gamma  tau =
  let vs = vars_of_clk_ct tau in
  let uv = unknowns_of_clk_env gamma in
  let genvars = Tools.make_set (substract vs uv) in
  replace_vars_by_base_ct tau;
  genvars, tau

let len clk =
  match clk with
  | Ck _ -> 1
  | CkTuple cts -> List.length cts

let mk_len_clock c n =
  let rec aux n acc=
    match n with
    | 0 -> CkTuple acc
    | _ -> aux (n-1) (c::acc)
  in
  if n = 1 then c
  else
    aux n []

let print_env env =
  let rec print_vars fmt gv =
    match gv with
    | [] -> ()
    | [v] -> Format.fprintf fmt "%d" v
    | v::vs -> Format.fprintf fmt "%d,%a" v print_vars vs
  in
  List.iter (fun (x,(gv,y)) ->
      Format.printf "\t forall %a . %s  => %a \n"
        print_vars gv
        x
        print_ct y) env

let get_ident e =
  match e.e_desc with
  | Variable n -> n
  | _ ->
    let s = Format.asprintf "%a : not an identifier" Parsing_ast_printer.print_expression e in
    Error.print_error e.e_loc s

(** Clk inference **)
let rec clk_expr delta (gamma : (string * clk_scheme) list) e =
  (* Format.printf "Clocking %a \n" Parsing_ast_printer.print_expression e; *)
  try
    begin
      match e.e_desc with
      | Call (f,e) ->
        let rec flatten_clocks = function
          | Ck c -> [c]
          | CkTuple ct ->
            List.fold_left (fun acc x -> flatten_clocks x @ acc) [] ct
          in
        let unify_all cks init =
          List.iter (fun c -> unify_ck c init) (flatten_clocks cks)
        in
        let t = clk_expr delta gamma e in
        let vr = new_varclk ()  in
        (* Unify all clocks of parameters together with vr: *)
        unify_all t.ce_clk vr;
        let ck = Ck vr in
        { ce_desc = CCall (f,t); ce_loc = e.e_loc; ce_clk = ck }
      | Unit ->
        let ck = Ck (new_varclk ()) in
        { ce_desc = CUnit; ce_loc = e.e_loc; ce_clk = ck }
      | Value v ->
        let ck = Ck (new_varclk ()) in
        { ce_desc = CValue v; ce_loc = e.e_loc; ce_clk = ck }
      | Variable n ->
        let sigma = try List.assoc n gamma
          with Not_found ->
            let s = Format.asprintf "Unbound variable : %s" n in
            Error.print_error e.e_loc s
        in
        let s = gen_instance_ct sigma in
        { ce_desc = CVariable n; ce_loc = e.e_loc; ce_clk = s }
      | PrefixOp (op,e) ->
        let t = clk_expr delta gamma e in
        { ce_desc = CPrefixOp (op,t) ; ce_loc = e.e_loc; ce_clk = t.ce_clk }
      | InfixOp (op,e1,e2) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        unify_ct t1 t2 None;
        { ce_desc = CInfixOp (op,t1,t2); ce_loc = e.e_loc; ce_clk = t1.ce_clk }
      | Alternative (e1,e2,e3) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        let t3 = clk_expr delta gamma e3 in
        unify_ct t1 t2 None;
        unify_ct t2 t3 None;
        { ce_desc = CAlternative (t1,t2,t3); ce_loc = e.e_loc; ce_clk = t1.ce_clk }
      | Fby (e1,e2) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        unify_ct t1 t2 None;
        { ce_desc = CFby (t1,t2); ce_loc = e.e_loc; ce_clk = t1.ce_clk }
      | Pre e ->
          let t = clk_expr delta gamma e in
          { ce_desc = CPre t; ce_loc = e.e_loc; ce_clk = t.ce_clk }
      | Arrow (e1,e2) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        unify_ct t1 t2 None;
        { ce_desc = CArrow (t1,t2); ce_loc = e.e_loc; ce_clk = t1.ce_clk }
      | When (e1,e2) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        let x = get_ident e2 in
        unify_ct t1 t2 None;
        let u = new_varclk () in
        let new_var = { ce_desc = CVariable x; ce_loc = e.e_loc; ce_clk = Ck u } in
        unify_ct new_var t1 None;
        { ce_desc = CWhen(t1,t2) ; ce_loc = e.e_loc; ce_clk = Ck (Ckon(u,x)) }
      | Application (id,num,ee) ->
        let (cks1,xs1,cks2,xs2) = try List.assoc id delta
          with Not_found ->
            let s = Format.asprintf "Unbound node : %s" id in
            Error.print_error e.e_loc s
        in
        (* Format.printf "Input clocks = %a\n" print_ct cks1; *)
        (* Format.printf "Output clocks = %a\n" print_ct cks2; *)
        let s = carriers_list cks1 @ carriers_list cks2 in
        let param = clk_expr delta gamma ee in
        let (gin,gout) = generalize_sign (cks1,cks2) in
        let params = match param.ce_desc with
          | CETuple e -> e
          | _ -> [param]
        in
        let xs1 = Tools.string_list_of_pattern xs1 in
        let sigma = get_subst xs1 params s in
        (* Format.fprintf Format.std_formatter "First subst ="; *)
        (* List.iter (fun (x,y) -> Format.fprintf Format.std_formatter "(%s -> %s)\n" x y) sigma; *)
        let gin = subst_ct gin sigma in
        let new_var = { ce_desc = CVariable id; ce_loc = e.e_loc; ce_clk = gin } in
        unify_ct new_var param (Some param.ce_loc);
        let xs2 = Tools.string_list_of_pattern xs2 in
        let sigma2 = begin try get_subst_vars xs2 !outputs s
          with Incorrect_params ->
            let s = "Incorrect number of input/output parameters" in
            Error.print_error e.e_loc s
        end
        in
        let gout = subst_ct gout sigma in
        let gout = subst_ct gout sigma2 in
        let c = get_cond_ct cks1 param.ce_clk in
        { ce_desc = CApplication(id,num,c,param);
          ce_loc = e.e_loc;
          ce_clk = gout }
      | Whennot (e1,e2) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        let x = get_ident e2 in
        let u = new_varclk () in
        let new_var = { ce_desc = CVariable x; ce_loc = e.e_loc; ce_clk = Ck u } in
        unify_ct new_var t1 None;
        unify_ct t1 t2 None;
        { ce_desc = CWhennot(t1,t2);
          ce_loc = e.e_loc ;
          ce_clk = Ck(Ckonnot(u,x))}
      | Merge (e1,e2,e3) ->
        let t1 = clk_expr delta gamma e1 in
        let t2 = clk_expr delta gamma e2 in
        let t3 = clk_expr delta gamma e3 in
        let x = get_ident e1 in
        let u = new_varclk () in
        let new_var = { ce_desc = CVariable x; ce_loc = e1.e_loc; ce_clk = Ck u } in
        unify_ct new_var t1 (Some e1.e_loc);
        let id2 = try get_ident e2 with _ -> "e2" in
        let x2 = "expected("^id2^")" in
        let new_var2 = { ce_desc = CVariable x2; ce_loc = e2.e_loc; ce_clk = (Ck (Ckon(u,x))) } in
        unify_ct  new_var2 t2 (Some e2.e_loc);
        let id3 = try get_ident e3 with _ -> "e3" in
        let x3 = "expected("^id3^")" in
        let new_var3 = { ce_desc = CVariable x3; ce_loc = e3.e_loc; ce_clk = (Ck (Ckonnot(u,x)))} in
        unify_ct new_var3 t3 (Some e3.e_loc);
        { ce_desc = CMerge (t1,t2,t3); ce_loc = e.e_loc; ce_clk = t1.ce_clk }
      | ETuple es ->
        let es' = List.map (clk_expr delta gamma) es in
        let c = CkTuple (List.map (fun e -> e.ce_clk) es') in
        { ce_desc = CETuple es'; ce_loc = e.e_loc ; ce_clk = c}
      | _ ->
        let s = Format.asprintf "%a : todo clocking" Parsing_ast_printer.print_expression e in
        Error.print_error e.e_loc s
    end
  with Unify_ct (c1,c2,e1,e2,loc) ->
    (* print_env gamma; *)
    let s = Format.asprintf "Clocking clash between %a::%a and %a::%a"
        print_expression e1 print_ct c1 print_expression e2 print_ct c2 in
    let loc = Option.value ~default:e.e_loc loc in
    Error.print_error loc s

let rec clk_expr_ct delta (gamma : (string * clk_scheme) list) e =
  match e.e_desc with
  | ETuple es ->
    let es' = List.map (clk_expr_ct delta gamma) es in
    let h = List.hd es' in
    { ce_desc = CETuple (List.map (clk_expr_ct delta gamma) es);
      ce_clk = h.ce_clk;
      ce_loc = e.e_loc}
  | _ ->  (clk_expr delta gamma e)


let rec lookup env p =
  match p.p_desc with
  | Ident i ->
    (try
       List.assoc i env
     with Not_found ->
       Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
    let clks = List.map (lookup env) t in
    let clk = CkTuple clks in clk
  | PUnit -> failwith "unit"
  | Typed (p,_t) -> lookup env p

let remove_types p =
  match p.p_desc with
  | Typed(p,_s) -> p
  | _ -> p


let split_tuple p =
  match p.p_desc with
  | Tuple pl -> List.map remove_types pl
  | _ -> [remove_types p]

let group_tuple pl =
  match pl with
  | [] -> Ck CkUnknown
  | [x] -> x
  | _ -> CkTuple pl


let rec assoc_env (env:(string * clk_scheme) list) p : clk_scheme =
  match p.p_desc with
  | Ident i ->
    (try
       List.assoc i env
     with Not_found ->
       Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
    let clks = List.map (assoc_env env) t in
    let clks = List.map (gen_instance_ct) clks in
    (* let clk = CkTuple clks in generalize_clk env clk *)
    ([],CkTuple clks)
  | PUnit -> failwith "unit"
  | Typed (p,_t) -> assoc_env env p


let clk_equations delta gamma eqs =
  let clk_eq (gamma:(string* clk_scheme) list) eq =
    outputs := Tools.string_list_of_pattern eq.pattern;
    let pck = assoc_env gamma eq.pattern in
    let pck = gen_instance_ct pck in
    let new_var = { ce_desc = CVariable "_"; ce_loc = eq.expression.e_loc; ce_clk = pck } in
    let cexp = clk_expr_ct delta gamma eq.expression in
    (try
       unify_ct new_var cexp None;
     with Unify_ct (c1,c2,_e1,_e2,_loc) ->
       let s = Format.asprintf "This expression has clock %a but %a was expected"
           print_ct c2 print_ct c1 in
       Error.print_error eq.expression.e_loc s);
    { cpattern = eq.pattern ;
      cexpression = cexp;
      cclock = cexp.ce_clk
    }
  in
  List.map (clk_eq gamma) eqs

let rec make_set l =
  match l with
  | [] -> []
  | x::xs ->
    if List.mem x xs then
      make_set l
    else
      x :: (make_set xs)

let get_all_inouts node =
  let ins = split_tuple node.inputs in
  let outs = split_tuple node.outputs in
  make_set (ins@outs)

let get_all_vars node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  let vars = List.flatten vars in
  let vars = List.map Tools.string_of_pattern vars in
  let inouts = split_tuple node.outputs in
  let inouts = List.map Tools.string_of_pattern inouts in
  let vars = substract vars inouts in
  make_set vars

let get_all_vars_pat node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  let vars = List.flatten vars in
  let inouts = split_tuple node.outputs in
  let vars = substract vars inouts in
  make_set vars

let lookup_clk env p =
  let s = Tools.string_of_pattern p in
  try
    List.assoc s env
  with Not_found ->
    Error.print_error p.p_loc ("Cannot find variable "^s)

exception Clock_escaping of string

let clk_node delta node clocks =
  let check_clock_escape clocks idents =
    let clock_is_in_idents ck ids = List.mem ck ids in
    List.iter (fun ck -> if not (clock_is_in_idents ck idents) then raise (Clock_escaping ck)) clocks
  in
  reset_varclk ();
  let inouts = get_all_inouts node in
  let inouts = List.map Tools.string_of_pattern inouts in
  let inout_clks =
    List.map (fun x -> (x,([],Ck (new_varclk ())))) inouts in
  let vars = get_all_vars node in
  let vars_clks =
    List.map (fun x -> (x,([],Ck (new_varclk())))) vars in
  let env = inout_clks@vars_clks in
  let eqs = clk_equations delta env node.equations in
  let ckins = List.map (fun x -> lookup_clk env x)
      (split_tuple node.inputs) in
  let ckins = List.map gen_instance_ct ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map (fun x -> lookup_clk env x)
      (split_tuple node.outputs) in
  let ckouts = List.map gen_instance_ct ckouts in
  let ckouts = group_tuple ckouts in
  let node_clk_ins = ckins in
  let node_clk_outs = ckouts in
  replace_vars_by_base_ct node_clk_ins;
  let node_clk_scheme = generalize_clk [] node_clk_outs in
  try
    let signs_in = List.map2 (fun x y -> (x,y)) (Tools.string_list_of_pattern node.inputs) (split_ct node_clk_ins) in
    let signs_out = List.map2 (fun x y -> (x,y)) (Tools.string_list_of_pattern node.outputs) (split_ct node_clk_outs) in
    let ct_in = List.map (fun x -> snd x) signs_in in
    let ct_out = List.map (fun x -> snd x) signs_out in
    let clocks_in = List.fold_left (fun acc x -> List.concat [Clocking_ast_printer.get_ct_clocks x; acc]) [] ct_in in
    let clocks_out = List.fold_left (fun acc x -> List.concat [Clocking_ast_printer.get_ct_clocks x; acc]) [] ct_out in
    let name_clocks = clocks_in@clocks_out in
    let print_sign fmt (x,y) =
      if List.mem x name_clocks then
        Format.fprintf fmt "[%s:%a]" x print_ct y
      else
        Format.fprintf fmt "%a" print_ct y
    in
    (* check that the clocks of all flows are defined in the signature of the node: *)
    (* TODO: make it so that this is detected earlier (ideally by the inference algo) *)
    let idents_in = List.map (fun x -> fst x) signs_in in
    let idents_outs = List.map (fun x -> fst x) signs_out in
    let all_idents = idents_in@idents_outs in
    check_clock_escape name_clocks all_idents;
    if clocks then
      begin
        Format.fprintf Format.std_formatter "%a :: "
          Parsing_ast_printer.print_pattern node.name;
        Tools.print_list Format.std_formatter print_sign ~sep:" * " signs_in;
        Format.fprintf Format.std_formatter " -> ";
        Tools.print_list Format.std_formatter print_sign  ~sep:" * " signs_out;
        Format.fprintf Format.std_formatter "\n";
      end;
    let clk1 = node_clk_ins in
    let clk2 = node_clk_outs in
    let xs1 = node.inputs in
    let xs2 = node.outputs in
    let globalenv = (Tools.string_of_pattern node.name,(clk1,xs1,clk2,xs2))::delta in
    let cnode =
      {
        cnode_clock = node_clk_scheme;
        cname = node.name;
        cinputs_clk = node_clk_ins;
        coutputs_clk = node_clk_outs;
        cinputs = node.inputs;
        coutputs = node.outputs;
        cequations = eqs
      } in
    (globalenv,env,cnode)
  with Clock_escaping id ->
    Error.print_error node.name.p_loc ("The following local clock is escaping its scope: "^id)
