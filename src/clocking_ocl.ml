open Parsing_ast
open Error

type clock = Unknown
           | Var of varclock
           | CTuple of clock list
           | Arrow of clock * clock
and varclock = { index : int ; mutable value : clock }
type clock_scheme = Forall of int list * clock

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
    | CTuple [] -> failwith "empty tuple"
    | CTuple (t::ts) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs t) ts
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
  List.flatten (List.map (function Forall(gv,t) -> unknowns_of_type t gv) env)

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
  let genvars = make_set genvars in
  Forall(genvars,tau)


(* Shorten variables (follow indirection) *)

let rec shorten t =
  match t with
  | Var { index = _ ; value = Unknown } -> t
  | Var { index = _ ; value = Var { index = m ; value = Unknown}} -> 
     Var { index = m ; value = Unknown }
  | Var ({ index = _ ; value = Var tv1} as tv2) ->
    tv2.value <- tv1.value;
    shorten t
  | Var { index = _ ; value = t' } -> t'
  | Unknown -> failwith "shorten"
  | _ -> t

(* Check if a variable occurs in a type  *)

let occurs { index = n ; value = _ } t =
  let rec occrec = function
    (* The variables have been shortened prior to calling this function *)
    | Var { index = m ; value = _} -> (n = m)
    | Arrow (t1,t2) -> (occrec t1) || (occrec t2)
    | CTuple ts -> List.fold_left (fun acc t -> acc || occrec t) false ts
    | Unknown -> failwith "occurs"
  in occrec t


(* Printing variables  *)
let cvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

(* Printing schemes  *)
let print_clock_scheme fmt (Forall(gv,t)) =
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
        with  Not_found -> failwith ("non generic var :"^string_of_int n))
      in
      let name = string_of_int n in
      Format.fprintf fmt "%s" name
    | Var { index = _ ; value = t } ->
      Format.fprintf fmt "*%a" print_rec t
    | Arrow(t1,t2) ->
      Format.fprintf fmt "( %a -> %a )" print_rec t1 print_rec t2
    | CTuple ts ->
      let rec print_list fmt l =
        match l with
          [] -> Format.fprintf fmt ""
        | [x] -> Format.fprintf fmt "%a" print_rec x
        | x::xs -> Format.fprintf fmt "%a,%a" print_rec x print_list xs
      in
      Format.fprintf fmt "(%a)" print_list ts
    | Unknown -> failwith "printclockscheme"
  in
  print_rec fmt t

(* Unification function : unifies types tau1 and tau2 *)
exception ClockClash of clock * clock

let rec unify (tau1,tau2) =
  let tau1 = shorten tau1 in
  let tau2 = shorten tau2 in
  Format.fprintf Format.std_formatter "J'unifie %a et %a \n" 
  print_clock_scheme (generalise_type !global_typing_env tau1)
  print_clock_scheme (generalise_type !global_typing_env tau2);
  match tau1, tau2 with
  | Var ({ index = n; value = Unknown} as tv1),
    Var { index = m; value = Unknown} ->
    (* two unknown variables *)
    (* tv1 -> tv2 -> <?> : every modif of tau2.value will modify tv1 *)
    if n <> m then tv1.value <- tau2;
  | _ , Var ({index = _ ; value = Unknown} as tv) ->
    if not (occurs tv tau1) then tv.value <- tau1
    else (raise (ClockClash (tau1,tau2)))
  | Var ({index = _ ; value = Unknown} as tv), _ ->
    if not (occurs tv tau2) then tv.value <- tau2
  | Arrow(t1,t2), Arrow(t1',t2') ->
    unify(t1,t1');
    unify(t2,t2')
  | CTuple tl1, CTuple tl2 ->
    List.iter2 (fun a b -> unify(a,b)) tl1 tl2;
  | _ -> raise (ClockClash (tau1,tau2))


(* Returns the variables in t that are not in bv (i.e. the free vars) *)
let unknowns_of_type t bv =
  substract (vars_of_clock t) bv

(* Get all the free vars in a type env  *)
let unknowns_of_type_env env =
  List.flatten (List.map (function Forall(gv,t) -> unknowns_of_type t gv) env)

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
let gen_instance (Forall(gv,tau)) =
  (* Each generic variables get unknown *)
  let unknowns = List.map (fun n -> n, Var(new_varclock ())) gv in
  let rec ginstance t =
    match t with
    | Var { index = n; value = Unknown } ->
      (* If the variable is generic get its corresponding unkown *)
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
  List.assoc n gamma
  
let print_env (env : env_elem list) =
  let rec print_list fmt l =
    match l with
      [] -> Format.fprintf fmt ""
    | [(p,s)] -> Format.fprintf fmt "(%a::%a)"
                   Parsing_ast_printer.print_pattern p
                   print_clock_scheme s
    | (p,s)::xs -> Format.fprintf fmt "(%a::%a),%a"
                 Parsing_ast_printer.print_pattern p
                 print_clock_scheme s
                 print_list xs
  in
  Format.fprintf Format.std_formatter "%a\n"
  print_list env

(* Get the clock of an ocalustre expression  *)
let clock_expr gamma e =
  let rec clock_rec e =
    match e.e_desc with
    | Value _ -> Var (new_varclock ())
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
      Format.fprintf Format.std_formatter "On trouve %s :: %a \n" 
                     n print_clock_scheme sigma;
      let gs = gen_instance sigma in 
(* Problem here : gen_instance creates new variables *)
      Format.fprintf Format.std_formatter "GenInstance : %a \n" 
                     print_clock_scheme (generalise_type !global_typing_env gs);
      gs
    | InfixOp (op,e1,e2) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      print_env gamma;
      print_clock_scheme Format.std_formatter (generalise_type !global_typing_env c1);
      print_clock_scheme Format.std_formatter (generalise_type !global_typing_env c2);
      unify(c1,c2);
      print_clock_scheme Format.std_formatter (generalise_type !global_typing_env c1);
      print_clock_scheme Format.std_formatter (generalise_type !global_typing_env c2);
      print_env gamma;
      c1
    | Alternative (e1,e2,e3) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      let c3 = clock_rec e3 in
      unify(c1,c2);
      unify(c2,c3);
      c3
    | Application(id,e') ->
      let u = Var (new_varclock ()) in
      let c1 = Var (new_varclock ()) (* todo : change *) in
      let c2 = clock_rec e' in
      unify(c1,Arrow(c2,u)); u
    | ETuple es -> CTuple (List.map clock_rec es)
    | _ -> failwith "type"
  in
  clock_rec e



let clocking ({ pattern = p; expression = e}) =
  let fmt = Format.std_formatter in
  let tau =
    try clock_expr !global_typing_env e
    with ClockClash (c1,c2) ->
      begin
        let vars = (vars_of_clock c1)@(vars_of_clock c2) in
        Format.fprintf fmt "Clash between %a and %a"
          print_clock_scheme (Forall(vars,c1))
          print_clock_scheme (Forall(vars,c2));
        failwith ":("
      end
  in
  let sigma = generalise_type !global_typing_env tau in
  (* global_env := s::!global_env; *)
  global_typing_env := (p,sigma)::!global_typing_env;
  (* reset_varclocks (); *)
  Format.fprintf fmt "Clock of %a is %a \n" Parsing_ast_printer.print_pattern p
    print_clock_scheme sigma

let rec grow_env p env =
    let clk = Var (new_varclock ()) in
    env := (p,(generalise_type !env clk))::!env

let split_tuple p =
  match p.p_desc with
  | Tuple pl -> pl
  | _ -> [p]

let rec look_for_ident env i =
  match env with
    [] -> raise Not_found
  | (p,s)::xs ->
    begin
      match p.p_desc with
      | Ident x ->
        if x = i then s
        else look_for_ident xs i
      | _ -> look_for_ident xs i
    end


let rec lookup_clock env p =
  match p.p_desc with
  | Ident i ->
    begin
    try
      look_for_ident env i
    with Not_found -> print_env env ; failwith ("not found :"^i)
  end
  | Tuple pl ->
    let clocks = List.map (fun p -> lookup_clock env p) pl in
    let clocks = List.map (fun (Forall(_,c)) -> c) clocks in
    let clk = CTuple clocks in
    generalise_type env clk
  | _ -> failwith "no"
  (* | Tuple pl -> List.fold_left (fun acc p -> (lookup_clock env p)@acc) [] pl *)


let clock_node node =
  reset_varclocks ();
  (* "Uncurrying" inputs and outputs ...   *)
  let ins = split_tuple node.inputs in
  (* let outs = split_tuple node.outputs in *)
  List.iter (fun p -> grow_env p global_typing_env) ins;
  (* List.iter (fun p -> grow_env p global_typing_env) outs; *)
  (* grow_env node.inputs global_typing_env; *)
  (* grow_env node.outputs global_typing_env; *)
  List.iter (fun eq -> clocking eq) node.equations;
  print_env !global_typing_env;
  let ins_p = List.map (fun { p_desc = Ident i; p_loc = _} -> i) ins in
  let ckins = List.map (look_for_ident !global_typing_env) ins_p in
  let ckins = List.map (fun (Forall(_,c)) -> c) ckins in
  let ckins = CTuple ckins in
  let ckins = generalise_type !global_typing_env ckins in
  (* let ckins = lookup_clock !global_typing_env node.inputs in *)
  let ckouts = lookup_clock !global_typing_env node.outputs in
  Format.fprintf Format.std_formatter "%a -> %a \n"
    print_clock_scheme ckins
    print_clock_scheme ckouts

let test () =
  let e = { e_desc = Value (Integer 5);
            e_loc = Location.none } in
  let e' = { e_desc = Value (Integer 5);
            e_loc = Location.none } in
  let e = { e with e_desc = ETuple [e;e'] } in
  let p = { p_desc = Ident "n";
            p_loc = Location.none } in
  let clk = clocking { pattern = p; expression = e } in
  ()
