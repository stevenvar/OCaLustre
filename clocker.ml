open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl
open Error

type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | CtUnknown
  | CVar of varclock
  | CTuple of ct list
  | Arrow of ct * ct
  | On of ct * carrier
  | Onnot of ct * carrier
  | Carrier of carrier
and varclock = { c_index : int ; mutable c_value : ct }
and scheme = Forall of int list * int list *  ct (* arrow ? *)
and carrier = { carr_index : int ; mutable carr_value : ct }
exception ClockingBug of string

exception CarrierClash of carrier * carrier

type env = (string * ct) list

let cpt = ref 0

let new_varclock, reset_varclocks =
  (function () -> incr cpt;
     { c_index = !cpt; c_value = CtUnknown }),
  (function () -> cpt := 0)

let carr = ref 0
let new_carrier, reset_carrier =
  (function () -> incr carr;
     { carr_index = !carr; carr_value = CtUnknown }),
  (function () -> carr := 0)


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
      in *) Format.fprintf Format.std_formatter "%d" n
  | CVar {c_index = n ; c_value = t} ->
    Format.fprintf Format.std_formatter "%a" print_clock t
  | Arrow (t1,t2) -> Format.fprintf fmt "%a -> %a" print_clock t1 print_clock t2
  | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_clock) tl
  | On (x,i) ->  Format.fprintf fmt "%a on %d" print_clock x i.carr_index
  | Onnot (x,i) -> Format.fprintf fmt "%a on (not %d)" print_clock x i.carr_index
  | CtUnknown -> Format.fprintf fmt  "?"
  | Carrier c ->
    Format.fprintf fmt "(%d : %a)" c.carr_index print_clock c.carr_value

let rec add_pat_to_env i env =
  match i.p_desc with
  | Ident id ->
    let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | PUnit -> let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | Tuple tl -> List.iter (fun i -> add_pat_to_env i env) tl

let rec add_to_env i tau env =
  match i.p_desc with
  | Ident id ->
    env := (i.p_desc,tau) :: !env ;
  | PUnit ->
    env := (i.p_desc,tau) :: !env ;
  | Tuple il ->
    match tau with
    | CTuple tl -> let lit = List.combine il tl in
      List.iter (fun (i,t) -> (add_to_env i t env)) lit
    | _ -> failwith "not a tuple ..."


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


let rec shorten_var t =
  match t with
  | CVar { c_index = _ ; c_value= CtUnknown} -> t
  | CVar ({ c_index = _ ; c_value = CVar {c_index = m ; c_value = CtUnknown}})->
    CVar {c_index = m ; c_value = CtUnknown}
  | CVar ({c_index = _ ; c_value = CVar tv1} as tv2) ->
    tv2.c_value <- tv1.c_value;
    shorten_var t
  | CVar {c_index = _ ; c_value = t'} -> t'
  | Carrier ({carr_index = n ; carr_value = CVar tv1} as tv2)->
    tv2.carr_value <- shorten_var tv2.carr_value;
    t
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
    | Carrier c -> false
    | _  -> raise (ClockingBug "occurs")
  in occrec



let rec unify (tau1, tau2) =
  match (shorten_var tau1, shorten_var tau2) with
  | (CVar ({c_index = n ;c_value =CtUnknown} as tv1) as t1),
    (CVar ({c_index = m ;c_value =CtUnknown} as tv2) as t2)
    ->


    if n <> m then (tv1.c_value <- t2 )

  | t1, (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.c_value <- t1
    else raise (ClockClash (t1,t2))
  | (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.c_value <- t2
    else raise (ClockClash (t1,t2))
  | Arrow (t1,t2) , Arrow (t1', t2')
    -> unify (t1,t1') ; unify (t2,t2')
  | CTuple ctl1 , CTuple ctl2 ->
    let ll = List.combine ctl1 ctl2 in
    List.iter unify ll
  | On (ct1,i1) , On (ct2, i2) ->
    unify (Carrier i1, Carrier i2);
    unify (ct1, ct2)
  | Onnot (ct1,i1) , Onnot (ct2, i2) ->
    unify (Carrier i1, Carrier i2);
    unify (ct1, ct2)
  | Carrier c , Carrier d  ->
    unify (c.carr_value, d.carr_value)
  | (t1,t2) -> raise (ClockClash (t1,t2))


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
    | Carrier c -> vars vs c.carr_value
    | CTuple [] -> assert false
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
    | On (x,i) -> i.carr_index::(vars vs x)
    | Onnot (x,i) -> i.carr_index::(vars vs x)
    | Carrier c -> c.carr_index :: vs
    | CTuple [] -> assert false
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
  let carriers = List.map (function n -> n, Carrier (new_carrier ())) gc in
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
    | On (x,i) ->
      begin
        try List.assoc i.carr_index carriers
        with _ -> failwith "not found" (* On(ginstance x,i) *)
      end
    | Onnot (x,i) ->
      begin
        try List.assoc i.carr_index carriers
        with _ -> failwith "not found" (* On(ginstance x,i) *)
      end
    | Carrier c ->
      let carr = new_carrier () in
      carr.carr_value <- ginstance c.carr_value;
      Carrier carr
    | CtUnknown -> raise (ClockingBug "inst")

  in ginstance ct


let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple pl -> List.fold_left (fun acc p -> (string_of_pattern p)@acc) [] pl
  | PUnit -> []


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
  let rec print_carr fmt c =
    let name = try List.assoc c.carr_index carr_name_list
      with Not_found ->
        raise (ClockingBug ("Non generic variable :"^(string_of_int c.carr_index)))
    in Format.fprintf Format.std_formatter "%s" name
  in
  let rec print_rec fmt = function
    | CVar {c_index = n ; c_value = CtUnknown } ->
      let name = try List.assoc n tvar_names
        with Not_found ->
          raise (ClockingBug ("Non generic variable :"^(string_of_int n)))
      in Format.fprintf Format.std_formatter "%s" name
    | CVar {c_index = _ ; c_value = t} -> print_rec fmt t
    | Arrow (t1,t2) -> Format.fprintf fmt "(%a -> %a)" print_rec t1 print_rec t2
    | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_rec) tl
    | On (x,i) ->  Format.fprintf fmt "%a on %a" print_rec x print_carr i
    | Onnot (x,i) ->  Format.fprintf fmt "%a on (not %a)" print_rec x print_carr i
    | CtUnknown -> Format.fprintf fmt  "?"
    | Carrier c -> Format.fprintf fmt "(%a : %a)" print_carr c print_rec c.carr_value
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
      let u = CVar (new_varclock ()) in
      let t1' = inst t1 in
      unify (Arrow (clock_rec e2, u),t1'); shorten_var u
    | ETuple t -> CTuple (List.map clock_rec t)
    | InfixOp (op, e1,e2) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      unify (t1,t2) ; t1
    | PrefixOp (op, e1) -> clock_rec e1
    | Unit ->
      CVar (new_varclock ())
    | Fby (e1,e2) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      unify (t1,t2); t1
    | Arrow (e1,e2) ->
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      unify (t1,t2) ; t1
    | Whennot (e1,e2) ->
    let carr =
    begin
      try
        List.assoc (get_ident e2) !carriers
      with  Not_found -> new_carrier ()
    end in
      let var = new_varclock () in
      carr.carr_value <- CVar var ;
      let t0 = Arrow (CVar var, Arrow (Carrier carr, Onnot (CVar var,carr))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let u = CVar (new_varclock ()) in
      unify (Arrow (t1, Arrow (t2,u)),t0);
      carriers := ((get_ident e2),carr) :: !carriers;
      shorten_var u
    | When (e1,e2) ->
      let carr =
      begin
        try
          List.assoc (get_ident e2) !carriers
        with  Not_found -> new_carrier ()
      end in
      let var = new_varclock () in
      carr.carr_value <- CVar var ;
      let t0 = Arrow (CVar var, Arrow (Carrier carr, On (CVar var,carr))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let u = CVar (new_varclock ()) in
      unify (Arrow (t1, Arrow (t2,u)),t0);
      carriers := ((get_ident e2),carr) :: !carriers;
      shorten_var u
    | Merge (e1,e2,e3) ->
      let var = CVar (new_varclock ()) in
      let carr1 =
      begin
        try
          List.assoc (get_ident e2) !carriers
        with  Not_found -> new_carrier ()
      end in
      carr1.carr_value <- var;
      let carr2 =
      begin
        try
          List.assoc (get_ident e3) !carriers
        with  Not_found -> new_carrier ()
      end in
      carr2.carr_value <- var;
      let t0 = Arrow (Carrier carr1, Arrow (On (var,carr1), Arrow ( Onnot (var,carr2) ,var))) in
      let t1 = clock_rec e1 in
      let t2 = clock_rec e2 in
      let t3 = clock_rec e3 in
      let u = CVar (new_varclock ()) in
      unify(Arrow (t1, Arrow (t2, Arrow(t3, u))),t0);

      shorten_var u
    | Pre e -> clock_rec e

  in
  clock_rec

let clocking_equation { pattern = p ; expression = e} =
  let tau =
    try typing_expr !typing_env e
    with ClockClash(t1,t2) ->
      let vars = (vars_of_clock t1)@(vars_of_clock t2) in
      let carrs = (carriers_of_clock t1)@(carriers_of_clock t2) in
      Format.fprintf Format.std_formatter
        " Clock clash between <%a and %a> "
        print_clock_scheme  (Forall(vars,carrs,t1))
        print_clock_scheme  (Forall(vars,carrs,t2));
      print_newline ();
      raise (Failure "clocking") in
  add_to_env p tau typing_env


let clock_node node tse =
  typing_env := [];
  carriers := []; 
  typing_scheme_env := !tse;
  reset_varclocks ();
  reset_carrier ();
  add_pat_to_env node.inputs typing_env;
  List.iter (fun e -> clocking_equation e) node.equations;
  List.iter (fun (e,c) -> print_string e; print_int c.carr_index  ) !carriers ;

  let lin =
    try get_clock node.inputs !typing_env
    with Not_found ->
      print_pattern Format.std_formatter node.inputs;
      failwith "lin"
  in
  let lout =
    try get_clock node.outputs !typing_env
    with Not_found -> print_pattern Format.std_formatter node.outputs;
      failwith "lout"
  in
  let tt = Arrow (lin, lout) in
  let ts = (generalise_clock (!typing_env,tt)) in
  Format.fprintf Format.std_formatter "%a :: %a\n" print_pattern node.name print_clock_scheme ts;

  (List.hd (string_of_pattern node.name), ts)::!typing_scheme_env
