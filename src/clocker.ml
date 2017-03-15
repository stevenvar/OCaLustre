open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl
open Clocking_ast
open Error

exception ClockingBug of string

exception CarrierClash of carrier * carrier

type env = (string * ct) list

let cpt = ref 0

let new_varclock, reset_varclocks =
  (fun () -> incr cpt; { c_index = !cpt; c_value = CtUnknown }),
  (fun () -> cpt := 0)

(*
let carr = ref 0
let new_carrier, reset_carrier =
  (fun () -> incr carr;
     { carr_index = !carr; carr_value = CtUnknown }),
  (fun () -> carr := 0)
*)

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be an ident"


let typing_env = ref []

let typing_scheme_env = ref []

let carriers = ref []

let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else ((name_of q)^s)
  in "'"^(name_of n)

let rec print_tuple f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a * " f h ; print_tuple f fmt t
  | _ -> ()


let rec print_clock fmt = function

  | CVar {c_index = n ; c_value = CtUnknown } ->
    (*let name = try List.assoc n tvar_names
      with Not_found ->
        raise (ClockingBug ("Non generic variable :"^(string_of_int n)))
      in *) Format.fprintf Format.std_formatter "%d?" n
  | CVar {c_index = n ; c_value = t} ->
    Format.fprintf Format.std_formatter "%a" print_clock t
  | Arrow (t1,t2) -> Format.fprintf fmt "%a -> %a" print_clock t1 print_clock t2
  | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_clock) tl
  | On (x,i) ->  Format.fprintf fmt "%a on %s" print_clock x i
  | Onnot (x,i) -> Format.fprintf fmt "%a on (not C%s)" print_clock x i
  | CtUnknown -> Format.fprintf fmt  "?"
  | Carrier (s,c) ->
    Format.fprintf fmt "(C%s : %a)" s print_clock c
  | CTyped (c,s) -> Format.fprintf fmt "(C%a:%s)" print_clock c s

let rec add_pat_to_env i env =
  match i.p_desc with
  | Ident id ->
    let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | PUnit -> let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | Tuple tl -> List.iter (fun i -> add_pat_to_env i env) tl
  | Typed (p,s) -> add_pat_to_env p env

let rec add_to_env i tau env =
  match i.p_desc with
  | Ident id ->
    env := (i.p_desc,tau) :: !env ;
  | PUnit ->
    env := (i.p_desc,tau) :: !env ;
  | Typed (p,s) ->
    add_to_env p tau env
  | Tuple il ->
    match tau with
    | CTuple tl -> let lit = List.combine il tl in
      List.iter (fun (i,t) -> (add_to_env i t env)) lit
    | _ -> Format.fprintf Format.std_formatter "--> %a" print_clock tau ; failwith "not a tuple ..."



let print_env fmt gamma =
  List.iter (fun (x,t) -> Format.fprintf Format.std_formatter "%a :: %a \n"
                print_pattern {p_desc = x ; p_loc = Location.none}
                print_clock t)
    gamma

let rec get_clock p env =
  match p.p_desc with
  | Ident i -> List.assoc p.p_desc env
  | PUnit -> List.assoc p.p_desc env
  | Tuple pl ->
    let tl = List.map (fun p -> List.assoc p.p_desc env) pl in
    CTuple tl
  | Typed (p',s) -> List.assoc p'.p_desc env


let rec shorten_var t =
  match t with
  | CVar { c_index = _ ; c_value= CtUnknown} -> t
  | CVar ({ c_index = _ ; c_value = CVar {c_index = m ; c_value = CtUnknown}})->
    CVar {c_index = m ; c_value = CtUnknown}
  | CVar ({c_index = _ ; c_value = CVar tv1} as tv2) ->
    tv2.c_value <- tv1.c_value;
    shorten_var t
  | CVar {c_index = _ ; c_value = t'} -> t'
 (* | Carrier ({carr_index = n ; carr_value = CVar tv1} as tv2)->
    tv2.carr_value <- shorten_var tv2.carr_value;
    t *)
  (* )  | CtUnknown -> raise (ClockingBug "shorten" ) *)
  | t' -> t'

exception ClockClash of ct * ct

let occurs {c_index = n; c_value = _ } =
  let rec occrec = function
    | CVar {c_index = m; c_value = _ } -> (n = m)
    | Arrow(ct1, ct2) -> occrec ct1 || occrec ct2
    | CTuple (ctl) -> List.fold_left (fun acc ctl -> occrec ctl || acc) false ctl
    | On (x,i) -> occrec x
    | Onnot (x,i) -> occrec x
    | Carrier (s,c) -> occrec c
    | _  -> raise (ClockingBug "occurs")
  in occrec



let rec unify (tau1, tau2) =

  match (shorten_var tau1, shorten_var tau2) with
  | (CVar ({c_index = n ;c_value =CtUnknown} as tv1) as t1),
    (CVar ({c_index = m ;c_value =CtUnknown} as tv2) as t2)
    ->

    if n <> m then (tv2.c_value <- t1 );

  | t1, (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.c_value <- t1
    else raise (ClockClash (t1,t2))
  | (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.c_value <- t2
    else raise (ClockClash (t1,t2))
  | Arrow (t1,t2) , Arrow (t1', t2') ->


     unify (t2,t2') ;  unify (t1,t1') ;


  | CTuple ctl1 , CTuple ctl2 ->
    let ll = List.combine ctl1 ctl2 in
    List.iter unify ll
  | On (ct1,i1) , On (ct2, i2) when i1 = i2 ->
    (* unify (Carrier i1, Carrier i2); *)
      unify (ct1, ct2)

  | Onnot (ct1,i1) , Onnot (ct2, i2) when i1 = i2 ->

    unify (ct1, ct2)
  | Carrier (s1,c1) , Carrier (s2,c2)  ->
    (* NE MARCHE PAS CAR ON NE VEUT PAS POUVOIR UNIFIER
       (a on c) et (a on d) *)
    (*    if c.carr_index = d.carr_index then *)
      unify (c1,c2)
  (*  else
      raise (ClockClash (Carrier c,Carrier d))*)
  | On (ct1,i1) , Carrier _ -> failwith "on carr"
  | k1,k2 -> raise (ClockClash (k1,k2))


let vars_of_clock tau =
  let rec vars vs ck =
    match ck with
    | CVar {c_index = n ; c_value = CtUnknown} ->
      if List.mem n vs then vs else n::vs
    | CVar {c_index = _ ; c_value = t} -> vars vs t
    | Arrow (t1,t2) -> vars (vars vs t1) t2
    | CTuple (c::ctl) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs c) ctl
    | On (x,i) -> vars vs x
    | Onnot (x,i) -> vars vs x
    | Carrier (s,c) -> vars vs c
    | CTuple [] -> assert false
    | CTyped (c,t) -> vars vs c
    | CtUnknown -> raise (ClockingBug "vars_of_clock")

  in vars [] tau

let carriers_of_clock tau =
  let rec vars vs ck =
    match ck with
    | CVar {c_index = n ; c_value = CtUnknown} -> vs
    | CVar {c_index = _ ; c_value = t} -> vars vs t
    | Arrow (t1,t2) -> vars (vars vs t1) t2
    | CTuple (c::ctl) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs c) ctl
    | On (x,i) -> i::(vars vs x)
    | Onnot (x,i) -> i::(vars vs x)
    | Carrier (s,c) -> (vars vs c)@vs
    | CTuple [] -> assert false
    | CTyped (c,t) -> vars vs c
    | CtUnknown -> raise (ClockingBug "carriers_of_clock")

  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_clock bv t = substract (vars_of_clock t) bv

let unknowns_of_clock_env env = List.flatten
    (List.map (fun (id,t) -> (unknowns_of_clock [] t)) env)

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalise_clock (gamma,tau) =
  let fv = vars_of_clock tau in
  let cv = carriers_of_clock tau in
  let genvars = make_set fv  in
  let carriers = make_set cv in
  Forall (genvars, carriers, tau)

let inst (Forall(gv,gc,ct)) =
  let unknowns = List.map (function n -> n, CVar (new_varclock ())) gv in
  let rec ginstance ct =
    match ct with
    | CVar { c_index = n ; c_value = CtUnknown } ->
      begin
        try List.assoc n unknowns
        with Not_found -> ct
      end
    | CVar {c_index = _ ; c_value = t } -> ginstance t
    | CTuple tl -> CTuple (List.map ginstance tl)
    | Arrow (t1,t2) -> Arrow (ginstance t1, ginstance t2)
    | On (x,i) -> On(ginstance x,i)
    | Onnot (x,i) -> Onnot (ginstance x,i)
    | Carrier (s,c) ->
      Carrier  (s, ginstance c)
    | CTyped (c,t) ->
      ginstance c
    | CtUnknown -> raise (ClockingBug "inst")

  in ginstance ct


let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple pl -> List.fold_left (fun acc p -> (string_of_pattern p)@acc) [] pl
  | PUnit -> []
  | Typed (p,s) -> string_of_pattern p


let carr_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else ((name_of q)^s)
  in "ck_"^(name_of n)


let print_clock_scheme fmt (Forall(gv,gc,t)) =
  let names = let rec names_of = function
      | (n,[]) -> []
      | (n,(v1::lv)) -> (tvar_name n)::(names_of (n+1,lv))
    in names_of (1,gv) in
  let carr_names = let rec names_of = function
      | (n,[]) -> []
      | (n,(v1::lv)) -> (carr_name n)::(names_of (n+1,lv))
    in names_of (1,gc) in
  let tvar_names = List.combine (List.rev gv) names in
  let carr_name_list = List.combine (List.rev gc) carr_names in
  let rec print_string_list fmt l =
    match l with
    | [] -> ()
    | [h] -> Format.fprintf fmt "%s" h
    | h :: t -> Format.fprintf fmt "%s,%a" h print_string_list t  in
  let print_forall fmt l =
    match l with
    |  [] -> ()
    |  _ -> Format.fprintf fmt "forall %a ." print_string_list l in

  let rec print_rec fmt = function
    | CVar {c_index = n ; c_value = CtUnknown } ->
      let name = try List.assoc n tvar_names
        with Not_found ->
          raise (ClockingBug ("Non generic variable :"^(string_of_int n)))
      in Format.fprintf Format.std_formatter "%s" name
    | CVar {c_index = _ ; c_value = t} -> print_rec fmt t
    | Arrow (t1,t2) -> Format.fprintf fmt "(%a -> %a)" print_rec t1 print_rec t2
    | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_rec) tl
    | On (x,i) ->  Format.fprintf fmt "%a on %s" print_rec x i
    | Onnot (x,i) ->  Format.fprintf fmt "%a on (not %s)" print_rec x i
    | CtUnknown -> Format.fprintf fmt  "?"
    | Carrier (s,c) -> Format.fprintf fmt "(%s : %a)" s print_rec c
    | CTyped (c,s) -> Format.fprintf fmt "(%a:%s)" print_rec c s
  in  Format.fprintf Format.std_formatter "%a %a %a" print_forall names print_forall carr_names
    print_rec t


let rec typing_expr gamma =
  let rec clock_rec { e_desc = e ; e_loc = _} =
    match e with
    | Value _ -> CVar (new_varclock () )
    | Variable n ->begin
        try
          let sigma =
            get_clock { p_desc = (Ident n) ; p_loc = Location.none } gamma in
          sigma
        with  Not_found -> CVar (new_varclock ())
      end
    | Alternative (e1,e2,e3) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let t3 = clock_rec e3 in
      unify (t1,t2);
      unify (t2,t3); t3
    | Application (i,e2) ->
      let t1 = List.assoc i !typing_scheme_env in
      let t2 = clock_rec e2 in
      let u = CVar (new_varclock ()) in
      let t1' = inst t1 in

      unify (Arrow (t2, u),t1');
       Format.fprintf  Format.std_formatter " t1' = %a ; u = %a ; t2 = %a \n "print_clock t1' print_clock u  print_clock t2;
      shorten_var u
    | ETuple t -> CTuple (List.map clock_rec t)
    | InfixOp (op, e1,e2) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      unify (t1,t2) ; t1
    | PrefixOp (op, e1) -> clock_rec e1
    | Unit ->
      CVar (new_varclock ())
    | Arrow (e1,e2) -> assert false
    | Fby (e1,e2) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      unify (t1,t2); t1
    | Whennot (e1,e2) ->
      let var = new_varclock () in
      let carr_name = get_ident e2 in
      let carr_ct = CVar (new_varclock ()) in
      let t0 = Arrow (CVar var, Arrow (Carrier (carr_name,carr_ct), Onnot (CVar var,carr_name))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let u = CVar (new_varclock ()) in
      unify (t1, carr_ct);
      unify (Arrow (t1, Arrow (t2,u)),t0);
      carriers := ((get_ident e2),carr_name) :: !carriers;
      shorten_var u
    | When (e1,e2) ->
      let var = new_varclock () in
      let carr_name = get_ident e2 in
      let carr_ct = CVar (new_varclock ()) in
      let t0 = Arrow (CVar var, Arrow (Carrier (carr_name, carr_ct), On (CVar var,carr_name))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let u = CVar (new_varclock ()) in
      unify (t1,carr_ct);
      unify (t2, carr_ct);
      unify (Arrow (t1, Arrow (t2,u)),t0);
      carriers := ((get_ident e2),carr_name) :: !carriers;
      shorten_var u
    | Merge (e1,e2,e3) ->
      let var = CVar (new_varclock ()) in
      let carr_name = get_ident e1 in
      let carr_ct = CVar (new_varclock ()) in
      let t0 = Arrow (Carrier (carr_name, carr_ct), Arrow (On (var,carr_name), Arrow ( Onnot (var,carr_name) ,var))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let t3 = clock_rec e3 in
      let u = CVar (new_varclock ()) in
      unify(Arrow (t1, Arrow (t2, Arrow(t3, u))),t0);

      shorten_var u
    | Call e ->
      let var = CVar (new_varclock ()) in
      let var2 = CVar (new_varclock ()) in
      let t0 = Arrow (var, var2) in
      let t =   CVar (new_varclock ()) in
      let u = CVar (new_varclock ()) in
      unify(t,t0);

      shorten_var u
  in
  clock_rec

let rec cpatt_of_patt { p_desc ; p_loc } cp_clock =
  let cp_desc =
  match p_desc with
  | Ident i -> CkIdent i
  | Tuple t -> let ct = List.map (fun t -> cpatt_of_patt t cp_clock) t in
    CkTuple ct
  | PUnit -> CkPUnit
  | Typed (p,s) -> (cpatt_of_patt p cp_clock).cp_desc
  in
  { cp_desc ; cp_loc = p_loc ; cp_clock}

let cexp_of_exp { e_desc ; e_loc } ce_clock =
  { ce_desc = e_desc; ce_loc = e_loc ; ce_clock }

let clocking_equation ({ pattern = p ; expression = e}) =
  let tau =

    try typing_expr !typing_env e
    with ClockClash(t1,t2) ->

      (*let vars = (vars_of_clock t1)@(vars_of_clock t2) in
        let carrs = (carriers_of_clock t1)@(carriers_of_clock t2) in *)
      Format.fprintf Format.std_formatter
        " Clock clash between <%a and %a> "
       (* print_clock_scheme  (Forall(vars,carrs,t1))
          print_clock_scheme  (Forall(vars,carrs,t2)); *)
        print_clock t1
        print_clock t2;
      print_newline ();
      raise (Failure "clocking") in

  add_to_env p tau typing_env;
  let cp = cpatt_of_patt p (Clock_exp tau) in
  let ce = { ce_desc = e.e_desc ; ce_loc = e.e_loc ; ce_clock = Clock_exp tau } in
  { cpattern = cp ; cexpression = ce }



  let rec remove_types p =
    match p.p_desc with
    | Ident x -> p
    | Tuple pl -> { p with p_desc =  Tuple (List.map remove_types pl)}
    | Typed (p',s) -> remove_types p'
    | PUnit -> p


let clock_node node tse clocking =
  typing_env := [];
  carriers := [];
  typing_scheme_env := !tse;
  reset_varclocks ();

  add_pat_to_env node.inputs typing_env;
  let inputs = remove_types node.inputs  in
  let outputs = remove_types node.outputs in

  let cequations = List.map (fun e -> clocking_equation e) node.equations in

  let lin =
    try get_clock inputs !typing_env
    with Not_found ->
      print_env Format.std_formatter !typing_env;
      print_pattern Format.std_formatter inputs;
      failwith "lin"
  in
  let lout =
    try get_clock outputs !typing_env
    with Not_found -> print_pattern Format.std_formatter outputs;
      failwith "lout"
  in
  let tt = Arrow (lin, lout) in
  let ts = (generalise_clock (!typing_env,tt)) in
  if !clocking then
  Format.fprintf Format.std_formatter "%s :: %a\n" node.name print_clock_scheme ts;

  List.hd (node.name, ts)::!typing_scheme_env
  ,
  { cname = node.name  ; cinputs = cpatt_of_patt node.inputs (Clock_exp lin) ; coutputs = cpatt_of_patt node.outputs (Clock_exp lout) ; cequations }
