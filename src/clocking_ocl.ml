open Parsing_ast

type clock = Unknown
           | Var of varclock
           | Arrow of clock * clock
and varclock = { index : int ; mutable value : clock }
type clock_scheme = Forall of int list * clock

let new_varclock, reset_varclocks =
  let cpt = ref 0 in
  (fun () ->
    incr cpt;
    { index = !cpt ; value = Unknown}
  ),
  fun () -> cpt := 0

(* Shorten variables (follow indirection) *)

let rec shorten t =
  match t with
  | Var { index = _ ; value = Unknown } -> t
  | Var { index = _ ; value = Var { index = _ ; value = Unknown} as tv} -> tv
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
    | Unknown -> failwith "occurs"
  in occrec t

(* Unification function : unifies types tau1 and tau2 *)
exception ClockClash of clock * clock

let rec unify (tau1,tau2) =
  let tau1 = shorten tau1 in
  let tau2 = shorten tau2 in
  match tau1, tau2 with
  | Var ({ index = n; value = Unknown} as tv1),
    Var { index = m; value = Unknown} ->
    (* two unknown variables *)
    (* tv1 -> tv2 -> <?> : every modif of tau2.value will modify tv1 *)
    if n <> m then tv1.value <- tau2
  | _ , Var ({index = _ ; value = Unknown} as tv) ->
    if not (occurs tv tau1) then tv.value <- tau1
    else (raise (ClockClash (tau1,tau2)))
  | Var ({index = _ ; value = Unknown} as tv), _ ->
    if not (occurs tv tau2) then tv.value <- tau2
  | Arrow(t1,t2), Arrow(t1',t2') ->
    unify(t1,t1');
    unify(t2,t2')
  | _ -> raise (ClockClash (tau1,tau2))

(* A typing env is a list of types of the program *)
(* let typing_env = ref [] *)

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

(* All the unknown vars of tau (except the ones in gamma) get bounded  *)
let generalise_type gamma tau =
  let genvars = (substract (vars_of_clock tau) (unknowns_of_type_env gamma)) in
  let genvars = make_set genvars in
  Forall(genvars,tau)

(* Create an instance of its type_env argument *)
let gen_instance (Forall(gv,tau)) =
  (* Each generic variables get unknown *)
  let unknowns = List.map (fun n -> n, Var(new_varclock ())) gv in
  let rec ginstance t =
    match t with
    | Var { index = n; value = Unknown } ->
      (* If the variable is generic get its corresponding unkown *)
      (try
         List.assoc n unknowns
       (* Otherwise stay free *)
       with Not_found -> t)
    | Var { index = _ ; value = t } ->
      (* follow indirection *)
      ginstance t
    | Arrow(t1,t2) -> Arrow(ginstance t1, ginstance t2)
    | Unknown -> failwith "gen_instance"
  in
  ginstance tau

(* Get the clock of an ocalustre expression  *)
let clock_expr gamma e =
  let rec clock_rec e =
    match e.e_desc with
    (* | Value _ -> Number *)
    | Value _ -> Var (new_varclock ())
    | Variable n ->
      (* get the clock scheme of n in the env *)
      let sigma =
        (try List.assoc n gamma
         with Failure _ -> failwith "Unbound")
        (* instantiate it *)
      in gen_instance sigma
    | Alternative (e1,e2,e3) ->
      let c1 = clock_rec e1 in
      let c2 = clock_rec e2 in
      let c3 = clock_rec e3 in
      unify(c1,c2);
      unify(c2,c3);
      c3
    | _ -> failwith "?"
  in
  clock_rec e

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
  let tvar_names = List.combine (List.rev gv) names in
  let rec print_rec fmt = function
    | Var { index = n ; value = Unknown } ->
      let name =
        (try List.assoc n tvar_names
        with  Not_found -> failwith "non generic var")
      in
      Format.fprintf fmt "%s" name
    | Var { index = _ ; value = t } -> print_rec fmt t
    | Arrow(t1,t2) ->
      Format.printf "( %a -> %a )" print_rec t1 print_rec t2
    | Unknown -> failwith "printclockscheme"
  in
  print_rec fmt t

let test () =
  reset_varclocks ();
  let e = { e_desc = Value (Integer 5);
            e_loc = Location.none } in
  let e = { e with e_desc = Alternative(e,e,e) } in
  let texp = clock_expr [] e in
  Format.fprintf Format.std_formatter "=>%a<=\n"  print_clock_scheme (Forall([0;1;2;3;4;5;6],texp))
