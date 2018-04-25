type clkvar = { index : int; mutable value : ck }
and clk_scheme = int list * ct

(** AST inspired from "Clock-directed modular code generation" **)

and ident = string

and ct =
  | Ck of ck
  | CTuple of ct list

and ck =
  | CBase
  | CUnknown
  | CVariable of clkvar
  | Con of ck * ident
  | Connot of ck * ident

open Parsing_ast

type cexpression = { ce_desc : exp_desc ; ce_clk : ct }

(** Printing **)

(* Printing variables  *)
let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

let rec print_ck fmt c =
  match c with
  | CBase -> Format.fprintf fmt "base"
  | CUnknown -> Format.fprintf fmt "?"
  | CVariable { index = n; value = CUnknown } ->
    Format.fprintf fmt "'%d" n
  | CVariable { index = n; value = c } ->
    Format.fprintf fmt "*%a" print_ck c
  | Con (ck,s) -> Format.fprintf fmt "%a on %s" print_ck ck s
  | Connot (ck,s) -> Format.fprintf fmt "%a onnot %s" print_ck ck s

let rec print_ct fmt c =
  let rec print_tuple fmt l =
    match l with
      [] -> ()
    | [c] -> Format.fprintf fmt "%a" print_ct c
    | c::cs -> Format.fprintf fmt "%a*%a" print_ct c print_ct c
  in
  match c with
  | Ck ck -> print_ck fmt ck
  | CTuple cks -> print_tuple fmt cks


(** Variables **)

let new_varclk, reset_varclk =
  let cpt = ref 0 in
  (fun () ->
     incr cpt;
     CVariable { index = !cpt ; value = CUnknown }),
  (fun () -> cpt := 0)

(** Shorten **)

exception Occurs


let occurs_ck { index = n; value = _} =
  let rec occrec = function
    | CVariable { index = m; value = _} -> if n = m then raise Occurs
    | CUnknown -> raise Occurs
    | CBase -> ()
    | Con (ck,x) -> occrec ck
    | Connot (ck,x) -> occrec ck
  in
  occrec

let rec occurs_ct var =
  function
  | Ck ck -> occurs_ck var ck
  | CTuple cts -> List.iter (fun s -> occurs_ct var s) cts

let rec shorten_ck c =
  match c with
  | CUnknown -> failwith "shorten"
  | CVariable { index = _; value = CUnknown} -> c
  | CVariable { index = _;
                   value = CVariable ({ index = _;
                                         value = CUnknown }) as tv} -> tv
  | CVariable ({ index = _; value = CVariable tv1 } as tv2) ->
    tv2.value <- tv1.value;
    shorten_ck c
  | CVariable { index = _ ; value = t' } -> shorten_ck t'
  | Con (ck,x) -> Con (shorten_ck ck, x)
  | Connot (ck,x) -> Con (shorten_ck ck, x)
  | _ -> c

let rec shorten_ct c =
  match c with
  | Ck ck -> Ck (shorten_ck ck)
  | CTuple cts -> CTuple (List.map shorten_ct cts)

(** Unify **)

exception Unify_ck of ck * ck
exception Unify_ct of ct * ct

let rec unify_ck c1 c2 =
  let t1 = shorten_ck c1 in
  let t2 = shorten_ck c2 in
  (* Format.printf "Unify %a and %a \n" print_clk c1 print_clk c2; *)
  try
    (match t1, t2 with
     | CBase, CBase -> ()
     | CVariable ({ index = n ; value = CUnknown } as tv1),
       CVariable ({ index = m; value = CUnknown } as tv2) ->
       if n <> m then tv1.value <- t2
     | CVariable ({ index = n ; value = CUnknown } as tv), c2 ->
       occurs_ck tv c2;
       tv.value <- c2
     | c1 , CVariable ({ index = n ; value = CUnknown } as tv) ->
       occurs_ck tv c1;
       tv.value <- c1
     | Con(ck,s), Con(ck',s') ->
       if s = s' then unify_ck ck ck'
       else raise (Unify_ck (t1,t2))
     | Connot(ck,s), Connot(ck',s') ->
       if s = s' then unify_ck ck ck'
       else raise (Unify_ck (t1,t2))
     | _ -> raise (Unify_ck (t1, t2)))
  with Occurs -> raise (Unify_ck (t1,t2))

let rec unify_ct c1 c2 =
  let c1 = shorten_ct c1 in
  let c2 = shorten_ct c2 in
  try
    (match c1,c2 with
     | Ck c1, Ck c2 -> unify_ck c1 c2
     | CTuple cts, Ck c ->
       List.iter (fun c -> unify_ct c c2) cts
     | CTuple cts, CTuple cts' ->
       (try
         List.iter2 (fun c d -> unify_ct c d) cts cts'
       with Invalid_argument _ -> Error.print_error Location.none "Not the same number of elements")

     | _ -> raise (Unify_ct (c1,c2)))
  with Unify_ck (_,_) -> raise (Unify_ct (c1,c2))

(** Instantiation **)

let gen_instance (gv,tau) =
  let gv_unknowns = List.map (fun n -> n, new_varclk ()) gv in
  let rec ginstance t =
    match t with
    | CVariable { index = n; value = CUnknown}->
      begin try
          List.assoc n gv_unknowns
        with Not_found -> t
      end
    | CVariable { index = _; value = t } -> ginstance t
    | CUnknown -> failwith "gen_instance"
    | Con (ck,x) -> Con(ginstance ck, x)
    | Connot (ck,x) -> Connot(ginstance ck, x)
    | CBase -> CBase
  in
  ginstance tau

let rec gen_instance_ct (gv,tau) =
  match tau with
  | Ck ck -> Ck (gen_instance (gv,ck))
  | CTuple cts -> CTuple (List.map (fun ct -> gen_instance_ct (gv,ct)) cts)

(** Generalization **)

(* returns clk vars, stream vars, carrier vars *)
let vars_of_clk tau =
  let rec vars vs v =
    match v with
    | CUnknown -> failwith "vars_of_clk"
    | CVariable { index = n; value = CUnknown} ->
      if List.mem n vs then vs else (n::vs)
    | CVariable { index = n; value = t } -> vars vs t
    | Con (ck,x) -> vars vs ck
    | Connot (ck,x) -> vars vs ck
    | CBase -> vs
  in vars [] tau

let vars_of_clk_ct tau =
  let rec vars vs v =
    match v with
    | Ck ck -> vars_of_clk ck
    | CTuple cts -> List.fold_left (fun acc ct -> vars acc ct) vs cts
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

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalize_clk gamma  tau =
  let vs = vars_of_clk_ct tau in
  let uv = unknowns_of_clk_env gamma in
  let genvars = make_set (substract vs uv) in
  genvars, tau

let len clk =
  match clk with
  | Ck ck -> 1
  | CTuple cts -> List.length cts

let mk_len_clock c n =
  let rec aux n acc=
    match n with
    | 0 -> CTuple acc
    | _ -> aux (n-1) (c::acc)
  in
  aux n []
(** Clk inference **)

let rec clk_expr (gamma : (string * clk_scheme) list) e =
  try
    begin
      match e.e_desc with
      | Unit -> CBase
      | Value _ -> CBase
      | Call _ -> CBase
      | Variable n ->
        let sigma = try List.assoc n gamma
          with Not_found ->
            let s = Format.asprintf "Unbound variable : %s" n in
            Error.print_error e.e_loc s
        in
        (match gen_instance_ct sigma with
        | Ck s -> s
        | CTuple cts -> failwith "ok")
      | PrefixOp (op,e) ->
        let t = clk_expr gamma e in
        t
      | InfixOp (op,e1,e2) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        unify_ck t1 t2;
        t1
      | Alternative (e1,e2,e3) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        let t3 = clk_expr gamma e3 in
        unify_ck t1 t2;
        unify_ck t2 t3;
        t1
      | Fby (e1,e2) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        unify_ck t1 t2;
        t1
      | Pre e -> clk_expr gamma e
      | Arrow (e1,e2) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        unify_ck t1 t2;
        t1
      | When (e1,e2) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        let x = "x" (* todo *) in
        unify_ck t1 t2;
        Con(t1,x)
      | Application (id,num,e) ->
          (* let fun_clock = clk_expr gamma { e_desc = Variable id; *)
        (* e_loc = Location.none} in *)
        let params = match e.e_desc with
          | ETuple  es -> es
          | _ -> [e]
        in
          let es = List.map (fun x -> clk_expr gamma x) params in
          (* unify_ck e fun_clock; *)
        List.hd es
        (* CTuple es *)
      | Whennot (e1,e2) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        let x = "x" (* todo *) in
        unify_ck t1 t2;
        Connot(t1,x)
      | Merge (e1,e2,e3) ->
        let t1 = clk_expr gamma e1 in
        let t2 = clk_expr gamma e2 in
        let t3 = clk_expr gamma e3 in
        let x = "x" (* todo *) in
        unify_ck (Con(t1,x)) t2;
        unify_ck (Connot(t1,x)) t3;
        t1
      | _ ->
        let s = Format.asprintf "%a : todo" Parsing_ast_printer.print_expression e in
        Error.print_error e.e_loc s
    end
  with Unify_ck (c1,c2) ->
    let s = Format.asprintf "Clocking clash between %a and %a"
        print_ck c1 print_ck c2 in
    Error.print_error e.e_loc s

let rec clk_expr_ct (gamma : (string * clk_scheme) list) e =
  try
  match e.e_desc with
  | ETuple es -> CTuple (List.map (clk_expr_ct gamma) es)
  | _ ->  Ck (clk_expr gamma e)
  with Unify_ck (c1,c2) -> raise (Unify_ct (Ck c1, Ck c2))



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

type cequation = { cpattern : Parsing_ast.pattern ; cexpression : cexpression }

let rec lookup env p =
   match p.p_desc with
  | Ident i ->
     (try
        List.assoc i env
      with Not_found ->
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
     let clks = List.map (lookup env) t in
     let clk = CTuple clks in clk
  | PUnit -> failwith "unit"
  | Typed (p,t) -> lookup env p

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
  | [] -> Ck CUnknown
  | [x] -> x
  | _ -> CTuple pl


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
    (* let clk = CTuple clks in generalize_clk env clk *)
    ([],CTuple clks)
  | PUnit -> failwith "unit"
  | Typed (p,t) -> assoc_env env p


let clk_equations gamma eqs =
  let rec clk_eq (gamma:(string* clk_scheme) list) eq =
    let clk = clk_expr_ct gamma eq.expression in
    let pck = assoc_env gamma eq.pattern in
    let pck = gen_instance_ct pck in
    (try
      unify_ct pck clk;
    with Unify_ct (c1,c2) ->
    let s = Format.asprintf "Clk clash between %a and %a"
        print_ct c1 print_ct c2 in
    Error.print_error eq.expression.e_loc s);
    { cpattern = eq.pattern ;
      cexpression = { ce_desc = eq.expression.e_desc; ce_clk = clk}
    }
  in
  List.map (clk_eq gamma) eqs



let get_all_inouts node =
  let ins = split_tuple node.inputs in
  let outs = split_tuple node.outputs in
  Clocks.make_set (ins@outs)

let get_all_vars node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  let vars = List.flatten vars in
  Clocks.make_set vars

let rec lookup_clk env p =
  let s = Tools.string_of_pattern p in
  try
    List.assoc s env
  with Not_found ->
    Error.print_error p.p_loc ("Cannot find variable "^s)

let clk_node gamma node =
  reset_varclk ();
  let vars = get_all_vars node in
  let vars = List.map Tools.string_of_pattern vars in
  let vars_clks =
    List.map (fun x -> (x,([],Ck (new_varclk())))) vars in
  let inouts = get_all_inouts node in
  let inouts = List.map Tools.string_of_pattern inouts in
  let inout_clks =
    List.map (fun x -> (x,([],Ck CBase))) inouts in
  let env = inout_clks@vars_clks@gamma in
  let eqs = clk_equations env node.equations in
  print_env env;
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
  (* print_env env; *)
  let node_clk_scheme = generalize_clk [] node_clk_outs in
  Format.printf "ins :: %a \n" print_ct node_clk_ins;
  Format.printf "outs :: %a \n" print_ct node_clk_ins;
  ((Tools.string_of_pattern node.name,node_clk_scheme))::gamma
