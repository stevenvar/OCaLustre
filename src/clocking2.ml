open Parsing_ast
open Clocking_ast2
open Parsing_ast_printer
 


exception CannotUnify of clock * clock
exception OccursCheckFailed of string * clock
exception UnknownIdentifier of string
exception InstantiationFail  

let typing_scheme_env = ref []
let typing_env = ref []

let rec free_vars c =
  match c with
  | CkUnknown -> [] 
  | CkVar a -> [a.index]
  | CkFun (a,b) -> (free_vars a) @ (free_vars b)
  | CkTuple cl -> List.fold_left (fun acc c -> (free_vars c @ acc)) [] cl
  | CkOn (a,b) -> (free_vars a) @ (free_vars b)
  | CkOnnot (a,b) -> (free_vars a) @ (free_vars b)
  | CkCarrier (_,a) -> free_vars a

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let free_vars_in_scheme (Forall (sl, c) ) = diff (free_vars c) sl 

let free_vars_in_env (scs) =
  List.fold_left (fun acc (s,cs) -> free_vars cs @ acc) [] scs

let rec shorten ck =
  match ck with
  | CkVar { index = _ ; value= CkUnknown} -> ck
  | CkVar { index = _ ; value = CkVar { index = m ; value = CkUnknown}} ->
    CkVar { index = m ; value = CkUnknown}
  | CkVar ({ index = _ ; value = CkVar tv1} as tv2) ->
    tv2.value <- tv1.value;
    shorten ck
  | CkVar { index = _ ; value = ck'} -> ck'
  | CkOn (a,b) -> CkOn (shorten a, shorten b)
  | _ -> ck

let rec unify (ck1,ck2) =
  let unify_binary (a,b) (x,y) =
    unify (a,x) ;  unify (b,y) 
  in
  match (shorten ck1), (shorten ck2) with
  | CkFun (a,b) , CkFun (c,d) -> unify_binary (a,b) (c,d)

  | CkVar v , x  -> if CkVar v <> x then v.value <- x
  | x , CkVar v -> if CkVar v <> x then v.value <- x ;
  | CkTuple cl1, CkTuple cl2 ->
    let ll = List.combine cl1 cl2 in
    List.iter unify ll
  | CkOn (a,b) , CkOn (c,d) ->
    unify_binary (a,b) (c,d)
  | CkOnnot (a,b) , CkOnnot (c,d) ->
    unify_binary (a,b) (c,d)
  | CkCarrier (s1,a), CkCarrier (s2,b) when s1 = s2 ->
    unify (a,b)
  | _ -> raise (CannotUnify (ck1,ck2) )

let fresh_var =
  let cpt = ref 0 in 
  fun () -> incr cpt ; CkVar { index = !cpt ; value = CkUnknown } 

let extend_env (Env e) (s,ck) = Env ((s,ck)::e)

let generalize env ck =
  let sl = diff (free_vars ck) (free_vars_in_env env) in
  Forall (sl,ck) 

let instantiate (Forall (ls,ck)) =
  let uns = List.map (function n -> n, fresh_var ()) ls in
  let rec ginstance ct =
    match ct with
    | CkVar { index = n ; value = CkUnknown } ->
      ( try List.assoc n uns
        with Not_found -> ct)
    | CkVar { index = _ ; value = cc } -> ginstance cc 
    | CkTuple cl -> CkTuple (List.map ginstance cl)
    | CkFun (a,b) -> CkFun (ginstance a , ginstance b)
    | CkOn (a,b) -> CkOn (ginstance a, ginstance b)
    | CkOnnot (a,b) -> CkOnnot (ginstance a, ginstance b)
    | CkCarrier (s,c) -> CkCarrier (s, ginstance c)
    | CkUnknown -> raise (InstantiationFail)
  in ginstance ck

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> failwith "This must be an ident"

(* VOIR si ça marche mieux si on dit que carrier c'est une variable 
(donc ajouter un champ carr_index aux ckVars *)
(* returns a type for the expression *)
let rec infer env e =
  let lookup_env e name =
    try List.assoc name e
    with Not_found -> raise (UnknownIdentifier name )
  in
  match e.e_desc with
  | Value v -> fresh_var ()
  | Variable n ->
    let sigma = lookup_env env n in
    Format.fprintf Format.std_formatter "%s : %a " n print_clock sigma; 
    sigma
  | Application (f,e) ->(
      try
        let ftau = List.assoc f !typing_scheme_env in 
        let etau = infer env e in 
        let fxtau = fresh_var () in
        unify (ftau , CkFun (etau, fxtau));
        fxtau
      with Not_found -> 
        raise (UnknownIdentifier f) )
  | When (e1,e2) ->
    let tau1 = infer env e1 in
    let tau2 = infer env e2 in
    let var = fresh_var () in 
    let tau_carr = fresh_var () in
    let carr = CkCarrier (get_ident e2 , tau_carr) in 
    let apptau = CkFun ( var , CkFun ( carr , CkOn (var, carr))) in
    
    unify (tau_carr, tau1);
    unify (tau_carr, tau2);
    let u = fresh_var () in 
    unify (CkFun (tau1, CkFun (tau2, u)), apptau);
    shorten u
    (*
    let tau2 = infer env e2 in
    let tau1 = infer env e1 in
    Format.fprintf Format.std_formatter "Before : tau1 = %a ; tau2 = %a @." print_clock tau1 print_clock tau2;
    unify (tau1,tau2);
    Format.fprintf Format.std_formatter "After : tau1 = %a ; tau2 = %a @." print_clock tau1 print_clock tau2;
    let tau3 = CkCarrier (get_ident e2 , tau2) in
    
    let t = CkOn (tau1, tau3) in
    shorten t
*)
  | Whennot (e1,e2) ->
    let tau1 = infer env e1 in
    let tau2 = infer env e2 in
    let var = fresh_var () in 
    let tau_carr = fresh_var () in
    let carr = CkCarrier (get_ident e2 , tau_carr) in 
    let apptau = CkFun ( var , CkFun ( carr , CkOnnot (var, carr))) in
    unify (tau_carr, tau1);
    unify (tau_carr, tau2);
    let u = fresh_var () in 
    unify (CkFun (tau1, CkFun (tau2, u)), apptau);
    shorten u
  | InfixOp (op, e1,e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    unify (t1,t2) ;
    shorten t2
  | Unit -> CkUnknown
  | Fby (e1,e2) ->
    let tau1 = infer env e1 in
    let tau2 = infer env e2 in
    unify (tau1,tau2);
    shorten tau1
  | Alternative (e1,e2,e3) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    let t3 = infer env e3 in
    unify (t1,t2);
    unify (t2,t3);
    shorten t3
  | PrefixOp (op, e1) -> infer env e1
  | ETuple t -> CkTuple (List.map (fun e -> infer env e) t)
  | Arrow (e1,e2) ->
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    unify (t1,t2);
    shorten t1
  | Merge (ck,e1,e2) ->
    let var = fresh_var () in 
    let carr = fresh_var () in 
    let carr_name = get_ident ck in 
    let t0 = CkFun (CkCarrier (carr_name, var), CkFun (CkOn (var,carr), CkFun ( CkOnnot (var,carr) ,var))) in
    let tck = infer env ck in
    let t1 = infer env e1 in
    let t2 = infer env e2 in
    let u = fresh_var ()  in
    unify(CkFun (tck, CkFun (t1, CkFun(t2, u))),t0);
    shorten var
  | Call _ -> fresh_var () 

let rec get_name p =
  match p.p_desc with
  | Ident i -> i
  | PUnit -> failwith "unit"
  | Typed (p,s) -> get_name p 
  | Tuple pt -> failwith "todo tuples"


let print_env fmt gamma =
  List.iter
    (fun (x,t) ->
       Format.fprintf Format.std_formatter "%s :: %a \n"
        x
        print_clock t)
    gamma


let rec add_pat_to_env i env =
  match i.p_desc with
  | Ident id ->
    let var = fresh_var () in 
    env := (id,var) :: !env ;
  | PUnit ->
    let var = fresh_var () in 
    env := ("()",var) :: !env ;
  | Tuple tl -> List.iter (fun i -> add_pat_to_env i env) tl
  | Typed (p,s) -> add_pat_to_env p env 
 

                
let ck_eq { pattern = p ; expression = e }  =
  let name =  get_name p in
  Format.fprintf Format.std_formatter "%s = %a \n @."
    name print_expression e;
  try
  let tau = infer !typing_env e in
  typing_env := (name,tau):: !typing_env ;
  let ce = { ce_desc = e.e_desc ;
             ce_loc = e.e_loc;
             ce_clock = tau } in
  { cpattern = cpatt_of_patt p tau ; cexpression = ce }
  with CannotUnify (ck1,ck2) ->
    Format.eprintf "Cannot unify %a and %a @." print_clock ck1 print_clock ck2;
    failwith "unify"

let rec clock_of_pat p env =
  match p.p_desc with
  | Ident i -> List.assoc i env
  | PUnit -> List.assoc "()" env
  | Tuple pl ->
    let tl = List.map (fun p -> clock_of_pat p env) pl in 
    CkTuple tl
  | Typed (p',s) -> clock_of_pat p' env


let ck_node n =
  typing_env := [];
  add_pat_to_env n.inputs typing_env;
  let cequations = List.map (fun e -> ck_eq e) n.equations in
  let ins = clock_of_pat n.inputs !typing_env in
  let outs = clock_of_pat n.outputs !typing_env in
  let tnode = CkFun(ins,outs) in
  let tscheme = generalize !typing_env tnode in 
  print_env Format.std_formatter !typing_env;
  Format.fprintf Format.std_formatter "%a :: %a\n" print_pattern n.name print_clock_scheme tscheme;
  { cname = cpatt_of_patt n.name tnode ;
    cinputs = cpatt_of_patt { p_desc = PUnit; p_loc = Location.none} CkUnknown;
    coutputs = cpatt_of_patt { p_desc = PUnit; p_loc = Location.none} CkUnknown;
    cequations }

