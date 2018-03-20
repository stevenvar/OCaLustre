open Parsing_ast
open Clocking_ast
open Error

(* The env is made of (name,type) couples for each variable *)
type env_elem = (pattern * clock_scheme)
(* type env_elem = (string * clock_scheme) *)

let new_varclock, reset_varclocks =
  let cpt = ref 0 in
  (fun () ->
    incr cpt;
    { index = !cpt ; value = Unknown}
  ),
  fun () -> cpt := 0


let new_varcar, reset_varcar =
  let cpt = ref 0 in
  (fun () ->
    incr cpt;
    { cindex = !cpt ; cvalue = UnknownCar}
  ),
  fun () -> cpt := 0


(* Harvest all the carriers in tau  *)
let carriers_of_clock tau =
  let rec cars vs t =
    match t with
    | UnknownCar -> failwith "cars_of_clock"
    | VarCar { cindex = n ; cvalue = UnknownCar } ->
      (* if the variable is already in the list, don't add *)
      if List.mem n vs then vs else n::vs
    | VarCar { cindex = _ ; cvalue = t } ->
      (* follow the indirection  *)
      cars vs t
  in
  let rec aux vs t =
    match t with
    | Var { index = n ; value = Unknown } -> []
    | Var { index = _ ; value = t } -> aux vs t
    | Arrow(t1,t2) ->
       aux (aux vs t1) t2
    | CTuple [] -> []
    | CTuple (t::ts) ->
       List.fold_left (fun acc t -> aux acc t) (aux vs t) ts
    | On (c,s) -> cars (aux vs c) s
    | Onnot (c,s) -> cars (aux vs c) s
    | Carrier (s,c) -> cars vs s
    | Unknown -> failwith "vars_of_clock"
  in
  aux [] tau

(* Harvest all the unknown variables in tau  *)
let vars_of_clock tau =
  let rec vars vs t =
    match t with
    | Var { index = n ; value = Unknown } ->
      (* if the variable is already in the list, don't add *)
      if List.mem n vs then vs else n::vs
    | Var { index = _ ; value = t } ->
      (* follow the indirection  *)
      vars vs t
    | Arrow(t1,t2) ->
      vars (vars vs t1) t2
    | CTuple [] -> []
    | CTuple (t::ts) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs t) ts
    | On (c,s) -> vars vs c
    | Onnot (c,s) -> vars vs c
    | Carrier (s,c) -> vars vs c
    | Unknown -> failwith "vars_of_clock"
  in
  vars [] tau

(* Function computing the diff of two lists  *)
  let substract l1 l2 =
    List.filter (fun x -> not (List.mem x l2)) l1

(* Returns the variables in t that are not in bv (i.e. the free vars) *)
let unknowns_of_type t bv =
  substract (vars_of_clock t) bv

(* Get all the free vars in a type env  *)
let unknowns_of_type_env env =
  List.flatten (List.map (function Forall(gv,gc,t) -> unknowns_of_type t gv) env)

(* We're working with sets  *)
let rec make_set l =
    match l with
    | [] -> []
    | x::xs ->
    if List.mem x xs then
      make_set l
    else
      x :: (make_set xs)

(* A typing env is a list of types of the program *)
(* let typing_env = ref [] *)
let global_typing_env : env_elem list ref = ref []

(* All the unknown vars of tau (except the ones in gamma) get bounded  *)
let generalise_type gamma tau =
  let gamma = List.map (fun (a,b) -> b) gamma in
  let genvars = (substract (vars_of_clock tau) (unknowns_of_type_env gamma)) in
  let gencars = carriers_of_clock tau in
  let genvars = make_set genvars in
  let gencars = make_set gencars in
  Forall(genvars,gencars,tau)


(* Shorten variables (follow indirection) *)
let rec shorten t =
  match t with
  | Var { index = _ ; value = Unknown } -> t
  | Var { index = _ ; value = (Var { index = m ; value = Unknown}) as tv } ->
    tv
  | Var ({ index = _ ; value = Var tv1} as tv2) ->
    tv2.value <- tv1.value;
    shorten t
  | Var { index = _ ; value = t' } -> t'
  | Carrier (s,c) -> Carrier(s,shorten c)
  | On (c,s) -> On(shorten c,s)
  | Onnot (c,s) -> Onnot(shorten c,s)
  | Unknown -> failwith "shorten"
  | _ -> t

(* Printing variables  *)
let cvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

(* Printing carriers *)
let car_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (65+r)) in
    if q = 0 then s else (name_of q)^s
  in "ck"^(name_of n)

let rec print_carrier fmt c =
  match c with
  | UnknownCar -> Format.fprintf fmt "?"
  | VarCar { cindex = n; cvalue = UnknownCar} -> Format.fprintf fmt "%s" (car_name n)
  | VarCar { cindex = n; cvalue = c} -> Format.fprintf fmt "%a" print_carrier c


let rec print_clock fmt (t,v) =
    let names =
    let rec names_of = function
      | (n,[]) -> []
      | (n, (_::xs)) -> (cvar_name n)::(names_of (n+1,xs))
    in
    names_of (1,v) in
  let cvar_names = List.combine (List.rev v) names in
  match t with
    | Var { index = n ; value = Unknown } ->
      let name =
        try
          List.assoc n cvar_names
        with Not_found -> string_of_int n in
       Format.fprintf fmt "%s" name
    | Var { index = m ; value = t } ->
      Format.fprintf fmt "*%a" print_clock (t,v)
    | Arrow(t1,t2) ->
      Format.fprintf fmt "(%a -> %a)" print_clock (t1,v) print_clock (t2,v)
    | CTuple ts ->
      let rec print_list fmt l =
        match l with
          [] -> Format.fprintf fmt ""
        | [x] -> Format.fprintf fmt "%a" print_clock (x,v)
        | x::xs -> Format.fprintf fmt "%a * %a" print_clock (x,v) print_list xs
      in
      Format.fprintf fmt "(%a)" print_list ts
    | On (c,s) -> Format.fprintf fmt "(%a on %a)" print_clock (c,v) print_carrier s
    | Onnot (c,s) -> Format.fprintf fmt "(%a on not %a)" print_clock (c,v) print_carrier s
    | Carrier (s,c) -> Format.fprintf fmt "(%a:%a)" print_carrier s print_clock (c,v)
    | Unknown -> Format.fprintf fmt  "?"

(* Printing schemes  *)
let print_clock_scheme fmt (Forall(gv,gc,t)) =
  (* Each bounded var gets a name *)
  let names =
    let rec names_of = function
      | (n,[]) -> []
      | (n, (_::xs)) -> (cvar_name n)::(names_of (n+1,xs))
    in
    names_of (1,gv) in
  let cvar_names = List.combine (List.rev gv) names in
  let rec print_rec fmt = function
    | Var { index = n ; value = Unknown } ->
      let name =
        (try List.assoc n cvar_names
         with  Not_found ->
           (* failwith ("non generic var :"^string_of_int n) *)
           string_of_int n
        )
      in
      Format.fprintf fmt "%s" name
    | Var { index = _ ; value = t } ->
      Format.fprintf fmt "%a" print_rec t
    | Arrow(t1,t2) ->
      Format.fprintf fmt "%a -> %a" print_rec t1 print_rec t2
    | CTuple ts ->
      let rec print_list fmt l =
        match l with
          [] -> Format.fprintf fmt ""
        | [x] -> Format.fprintf fmt "%a" print_rec x
        | x::xs -> Format.fprintf fmt "%a * %a" print_rec x print_list xs
      in
      Format.fprintf fmt "(%a)" print_list ts
    | On (c,s) -> Format.fprintf fmt "(%a on %a)" print_rec c print_carrier s
    | Onnot (c,s) -> Format.fprintf fmt "(%a on not %a)" print_rec c print_carrier s
    | Carrier (s,c) -> Format.fprintf fmt "(%a:%a)" print_carrier s print_rec c
    | Unknown -> Format.fprintf fmt  "?"
  in
  let rec print_list fmt l =
    match l with
    | [] -> Format.fprintf fmt ""
    | [x] -> Format.fprintf fmt "%s"
               (List.assoc x cvar_names)
    | x::xs -> Format.fprintf fmt "%s,%a"
                 (List.assoc x cvar_names)
                 print_list xs
  in
  Format.fprintf fmt "forall %a. %a" print_list (List.rev gv)  print_rec t

(* Unification function : unifies types tau1 and tau2 *)
exception ClockClash of clock * clock



(* Shorten variables (follow indirection) *)
let rec car_shorten c =
  match c with
  | VarCar { cindex = _ ; cvalue = UnknownCar } -> c
  | VarCar { cindex = _ ; cvalue = (VarCar { cindex = m ; cvalue = UnknownCar}) as tv } ->
    tv
  | VarCar ({ cindex = _ ; cvalue = VarCar tv1} as tv2) ->
    tv2.cvalue <- tv1.cvalue; car_shorten c
  | UnknownCar -> failwith "shorten"


let caroccurs { cindex = n ; cvalue = _ } c =
  let rec occrec c =
    Format.fprintf Format.std_formatter "Occurs %d in %a ? \n%!"
      n
      print_carrier c;
    let c = car_shorten c in
  match c with
    | VarCar { cindex = m ; cvalue = _} -> (n = m)
    | UnknownCar -> failwith "occurs"
  in occrec c


(* Check if a variable occurs in a type  *)
let occurs { index = n ; value = _ } t =
  let rec occrec c =
    (* Format.fprintf Format.std_formatter "Occurs %d in %a ? \n%!" *)
      (* n *)
      (* print_clock (c,[]); *)
    let c = shorten c in
  match c with
    | Var { index = m ; value = _} -> (n = m)
    | Arrow (t1,t2) -> (occrec t1) || (occrec t2)
    | CTuple ts -> List.fold_left (fun acc t -> acc || occrec t) false ts
    | On (c,s) -> occrec c
    | Onnot (c,s) -> occrec c
    | Carrier (s,c') -> occrec c'
    | Unknown -> failwith "occurs"
  in occrec t

let unify_carriers (c1,c2) =
  let c1,c2 = car_shorten c1, car_shorten c2 in
  match c1,c2 with
  | VarCar ({ cindex = n; cvalue = UnknownCar } as cv1), VarCar { cindex = m ; cvalue = UnknownCar } ->
    if n <> m then cv1.cvalue <- c2
  | _ , VarCar ({cindex = _; cvalue = UnknownCar} as cv) ->
    if not (caroccurs cv c1) then cv.cvalue <- c1
    else failwith "caroccurs"
  | VarCar ({cindex = _; cvalue = UnknownCar} as cv), _ ->
    if not (caroccurs cv c2) then cv.cvalue <- c2
    else failwith "carrocurs"
  | _ -> failwith "unify_carriers"

let rec unify (tau1,tau2) =
  let tau1 = shorten tau1 in
  let tau2 = shorten tau2 in
  (* Format.fprintf Format.std_formatter "Unifying %a and %a \n%!" *)
  (* print_clock (tau1,vars_of_clock tau1) *)
  (* print_clock (tau2,vars_of_clock tau2); *)
  begin
    match tau1, tau2 with
    | Carrier (s,c) , Carrier (s',d) ->
      unify_carriers(s,s');
      unify(c,d);

    | Var ({ index = n; value = Unknown} as tv1),
      Var { index = m; value = Unknown} ->
      (* two unknown variables *)
      (* tv1 -> tv2 -> <?> : every modif of tau2.value will modify tv1 *)
      if n <> m then
        tv1.value <- tau2;
    | _ , Var ({index = _ ; value = Unknown} as tv) ->
      if not (occurs tv tau1) then tv.value <- tau1
      else (raise (ClockClash (tau1,tau2)))
    | Var ({index = m ; value = Unknown} as tv), _ ->
      if not (occurs tv tau2) then
        tv.value <- tau2
      else (raise (ClockClash (tau1,tau2)))
    | Arrow(t1,t2), Arrow(t1',t2') ->
      unify(t1,t1');
      unify(t2,t2')
    | CTuple tl1, CTuple tl2 ->
      List.iter2 (fun a b -> unify(a,b)) tl1 tl2;
    | Onnot (c,x) , Onnot (d,y) ->
      unify_carriers(x,y);
      unify(c,d)
    | On (c,x) , On (d,y) ->
      unify_carriers(x,y);
      unify(c,d);
    | Carrier (s,c) , _ ->
       unify(c,tau2)
    | _ , Carrier (s,c) -> unify(tau1,c)
   | _ -> raise (ClockClash (tau1,tau2))
  end

  (* Format.fprintf Format.std_formatter "After unifying : %a and %a \n" *)
  (* print_clock_scheme (generalise_type !global_typing_env tau1) *)
  (* print_clock_scheme (generalise_type !global_typing_env tau2) *)


(* Returns the variables in t that are not in bv (i.e. the free vars) *)
let unknowns_of_type t bv =
  substract (vars_of_clock t) bv

(* Get all the free vars in a type env  *)
let unknowns_of_type_env env =
  List.flatten (List.map (function Forall(gv,gc,t) -> unknowns_of_type t gv) env)

(* We're working with sets  *)
let rec make_set l =
    match l with
    | [] -> []
    | x::xs ->
    if List.mem x xs then
      make_set l
    else
      x :: (make_set xs)

(* Create an instance of its type_env argument *)
let gen_instance (Forall(gv,gc,tau)) =
  (* Each generic variables get unknown *)
  let unknowns = List.map (fun n -> n, Var(new_varclock ())) gv in
  let cunknowns = List.map (fun n -> n, VarCar(new_varcar ())) gc in
  let rec carinstance c =
    match c with
    | VarCar { cindex = n; cvalue = UnknownCar } ->
       (try List.assoc n cunknowns with Not_found -> c)
    | VarCar { cindex = _ ; cvalue = t } -> carinstance t
    | UnknownCar -> failwith "car instance"
  in
  let rec ginstance t =
    match t with
    | Var { index = n; value = Unknown } ->
      (* If the variable is generic get its corresponding unknown *)
      begin
        try
          List.assoc n unknowns
      (* Otherwise stay free *)
        with Not_found -> t
      end
    | Var { index = _ ; value = t } ->
      (* follow indirection *)
      ginstance t
    | Arrow(t1,t2) -> Arrow(ginstance t1, ginstance t2)
    | CTuple ts -> CTuple (List.map ginstance ts)
    | On (c,s) -> On (ginstance c,carinstance s)
    | Onnot (c,s) -> Onnot (ginstance c,carinstance s)
    | Carrier (s,c) -> Carrier (carinstance s,ginstance c)
    | Unknown -> failwith "gen_instance"
  in
  ginstance tau

let clock_lookup (n:string) (gamma: env_elem list) =
  let rec get_idents l =
    match l with
    | [] -> []
    | ({ p_desc = Ident x; p_loc = _},y)::xs ->
       (x,y)::(get_idents xs)
    | _::xs ->
       (get_idents xs)
  in
  let gamma = get_idents gamma in
  try
    List.assoc n gamma
  with Not_found ->
    let s = Format.asprintf "Not found : %s" n in
    Error.print_error Location.none s

let print_env fmt (env : env_elem list) =
  let rec print_list fmt l =
    match l with
      [] -> Format.fprintf fmt ""
    | [(p,s)] -> Format.fprintf fmt "(%a:%a)"
                   Parsing_ast_printer.print_pattern p
                   print_clock_scheme s
    | (p,s)::xs -> Format.fprintf fmt "(%a:%a),%a"
                 Parsing_ast_printer.print_pattern p
                 print_clock_scheme s
                 print_list xs
  in
  Format.fprintf fmt "Γ = {%a}\n"
    print_list env

let get_ident e =
  match e.e_desc with
  | Variable x -> x
  | _ -> failwith "nope"

let make_carrier s ck = Carrier (s,ck)

(* Get the clock of an ocalustre expression  *)
let clock_expr gamma e =
  let rec clock_rec e =
    match e.e_desc with
    | Value _ | Unit -> Var (new_varclock ())
    | Array e -> clock_rec (List.hd e)
    | Array_get (e,_) -> clock_rec e
    | Array_fold (e,_,acc) ->
      let c1 = clock_rec e in
      let c2 = clock_rec acc in
      unify(c1,c2);
      c1
    | Array_map (e,_) ->
      clock_rec e
    | Imperative_update (e,_) -> clock_rec e
    | Variable n ->
      (* get the clock scheme of n in the env *)
      let sigma =
        begin
          try clock_lookup n gamma
          with Not_found ->
            Error.print_error e.e_loc ("Unbound variable "^n)
        end
        (* instantiate it *)
      in
      let gs = gen_instance sigma in
      gs
    | InfixOp (op,e1,e2) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      unify(c1,c2);
      c1
    | Alternative (e1,e2,e3) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      let c3 = clock_rec e3 in
      unify(c1,c2);
      unify(c2,c3);
      c3
    | Application(id,e') ->
       begin
       try
         let fun_ck = clock_lookup id !global_typing_env in
         let fun_ck = gen_instance fun_ck in
         let arg_ck = clock_rec e' in
         let u = Var (new_varclock ()) in
         let t = Arrow (arg_ck,u) in
         unify(t,fun_ck);
         u
       with Not_found ->
             Error.print_error e.e_loc "Node not found"
       end
    | When (e,c) ->
      let c1 = clock_rec e in
      let c2 = clock_rec c in
      (* Clock of 'when' is 'a -> (c :: 'a) -> 'a on c *)
      let a = Var (new_varclock ()) in
      let s = VarCar(new_varcar ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),On(a,s)))) in
      (* clock of result *)
      let u = Var (new_varclock ()) in
      let new_type = Arrow(c1,Arrow(c2,u)) in
      unify(tt,new_type);
      u
    | Whennot (e,c) ->
      let c1 = clock_rec e in
      let c2 = clock_rec c in
      (* Clock of 'whenot' is 'a -> (c :: 'a) -> 'a on (not c) *)
      (* let s = get_ident c in *)
      (* let s = "clk_"^s in *)
      let a = Var (new_varclock ()) in
      let s = VarCar(new_varcar ()) in
      let tt = (Arrow (a,Arrow(Carrier(s,a),Onnot(a,s)))) in
      (* clock of result *)
      let u = Var (new_varclock ()) in
      let new_type = Arrow(c1,Arrow(c2,u)) in
      unify(tt,new_type);
      u
    | Merge (c,e1,e2) ->
      let cc = clock_rec c in
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      (* let s = "clk_"^s in *)
      (* Clock of 'whenot' is clk:'a -> 'a on c -> 'a on (not c) -> 'a *)
      let a = Var(new_varclock () ) in
      let s = VarCar(new_varcar ()) in
      let tt = Arrow(Carrier(s,a), Arrow(On(a,s),Arrow(Onnot(a,s),a))) in
      let u = Var (new_varclock () ) in
      let new_type = Arrow(cc,Arrow(c1,Arrow(c2,u))) in
      unify(tt,new_type);
      u
    | ETuple es -> CTuple (List.map clock_rec es)
    | Fby (e1,e2) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      unify(c1,c2);
      c1
    | PrefixOp (op,e) ->
      let c = clock_rec e in
      c
    | Call e ->
      let a = Var(new_varclock ()) in
      a
    | Pre _ | Arrow _ -> failwith "shouldn't happen"
  in
  clock_rec e

let clocking gamma ({ pattern = p; expression = e}) =
  let tau =
    try clock_expr gamma e
    with ClockClash (c1,c2) ->
      begin
        let vars1 = (vars_of_clock c1)
        in let vars2 = (vars_of_clock c2) in
        let s1 = Format.asprintf
        "Clock clash between %a and %a"
          print_clock_scheme (Forall(vars1,[],c1))
          print_clock_scheme (Forall(vars2,[],c2)) in
        Error.print_error e.e_loc s1
      end
  in
  let tau = shorten tau in
  let sigma = generalise_type gamma tau in
  (p,sigma)::gamma


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

(* Helper function that computes the position of an element in a list *)
let get_position y l =
  let rec aux l n =
    match l with
    | [] -> failwith "get_position"
    | x::xs ->
      if y = x then n
      else
        aux xs (n+1)
  in
  aux l 0

(* Function looking for a variable in an env (goes into tuples)  *)
let rec look_for_ident (env : env_elem list) i =
  match env with
    [] -> (failwith ("cannot find "^i))
            (* raise Not_found *)
  | (p,s)::xs ->
    begin
      match p.p_desc with
      | Ident x ->
        if x = i then s
        else look_for_ident xs i
      | Tuple tl ->

        let tl = List.map (fun { p_desc = p } -> p) tl in
        begin
          try
            let n = get_position (Ident i) tl in
            match s with
            | Forall(g,c,CTuple l) -> Forall(g,c,List.nth l n)
            | _ -> failwith "look for ident"
          with Not_found ->
            look_for_ident xs i
        end
      | _ ->
        look_for_ident xs i
    end

open Parsing_ast_printer

(* Be careful not to look for the same location (bug for outputs) *)
let rec pattern_equal p1 p2 =
  match p1.p_desc, p2.p_desc with
  | Ident i, Ident j -> i = j
  | Typed (pi,_) , Typed (pj,_) -> pattern_equal pi pj
  | Tuple pi, Tuple pj ->
     let l = List.map2 (fun i j -> (i,j)) pi pj in
     List.fold_left (fun acc (x,y) -> (pattern_equal x y) && acc) true l
  | PUnit, PUnit -> true
  | _ -> false

let rec assoc_env p env =
  match env with
  |[] -> raise Not_found
  | (x,c)::xs -> if pattern_equal x p then c else
      assoc_env p xs

(* Function for finding clock in an env *)
let rec lookup_clock env p =
  match p.p_desc with
  | Ident i ->
     (try
        assoc_env p env
      with Not_found ->
        (* generalise_type env (Var (new_varclock ()))) *)
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
     let clocks = List.map (lookup_clock env) t in
     let clocks = List.map gen_instance clocks in
     let clk = CTuple clocks in
     generalise_type env clk
  | PUnit -> assoc_env p env
  | Typed (p,t) -> try assoc_env p env with
    Not_found ->
    let s = Format.asprintf "Not found : %a"  print_pattern p in
    Error.print_error p.p_loc s
  (* | _ -> *)
     (* let s = Format.asprintf "%a" print_pattern p in *)
     (* Error.print_error p.p_loc s *)
  (* try
   *   let x = assoc_env p env in
   *   x
   * with Not_found ->
   *   raise (Location.Error (Location.error ~loc:p.p_loc "Cannot find clock"))
   *   (\* failwith "lookup_clock" *\) *)
  (* match p.p_desc with
   * | Ident i ->
   *   begin
   *   try
   *     look_for_ident env i
   *   with Not_found -> failwith ("not found :"^i)
   * end
   * | Tuple pl ->
   *   let clocks = List.map (fun p -> lookup_clock env p) pl in
   *   let clocks = List.map (fun (Forall(_,c)) -> c) clocks in
   *   let clk = CTuple clocks in
   *   generalise_type env clk
   * | _ -> Format.fprintf Format.std_formatter "(%a)" Parsing_ast_printer.print_pattern p ;  failwith "Don't know what to do with : " *)


let get_all_vars node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  let vars = List.flatten vars in
  let ins = split_tuple node.inputs in
  (* let outs = split_tuple node.outputs in *)
  make_set (vars@ins)


let clock_node node =
  reset_varclocks ();
  let vars = get_all_vars node in
  (* vars_of_clock ?  *)
  let vars_clocks = List.map (fun x -> (x,Forall([],[],Var(new_varclock())))) vars in
  let env = vars_clocks@(!global_typing_env) in
  let env = List.fold_left (fun acc eq -> clocking acc eq) env node.equations in
  let ckins = List.map (fun x -> lookup_clock env x) (split_tuple node.inputs) in
  let ckouts = List.map (fun x -> lookup_clock env x) (split_tuple node.outputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_type = Arrow ( ckins, ckouts) in
  let scheme = generalise_type [] node_type in
  Format.fprintf Format.std_formatter "%a :: %a\n"
    print_pattern node.name
    print_clock_scheme scheme;
  global_typing_env := (node.name,scheme)::!global_typing_env

(* let _ =
 *   let a = Var(new_varclock ()) in
 *   let b = Var(new_varclock ()) in
 *   let c = VarCar(new_varcar ()) in
 *   let a = On(a,c) in
 *   let d = Carrier(VarCar(new_varcar ()), b) in
 *   Format.printf "Before : %a and %a \n" print_clock (a,[])  print_clock (d,[]);
 *   unify(a,d);
 *   Format.printf "After : %a and %a \n" print_clock (a,[])  print_clock (d,[]); *)
