open Parsing_ast
open Clocking_ast
open Error
open Carriers

(* The env is made of (name,type) couples for each variable *)
type env_elem = (string * clock_scheme)

let new_varclock, reset_varclocks =
  let cpt = ref 0 in
  (fun () -> incr cpt; { index = !cpt ; value = Unknown}),
  fun () -> cpt := 0

(* Harvest all the unknown variables in tau  *)
let vars_of_clock tau =
  let rec vars vs t =
    match t with
    | Base -> []
    | Var { index = n ; value = Unknown } ->
      (* if the variable is already in the list, don't add *)
      if List.mem n vs then vs else n::vs
    | Var { index = _ ; value = t } ->
      vars vs t
    | Arrow(t1,t2) ->
      vars (vars vs t1) t2
    | CTuple [] -> []
    | CTuple (t::ts) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs t) ts
    | On (c,s) -> vars vs c
    | Onnot (c,s) -> vars vs c
    | Carrier (s,c) -> vars vs c
    | Unknown -> []
    (* | Unknown -> Error.print_error Location.none "vars_of_clock" *)
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

let rec full_shorten t =
  let shorten = full_shorten in
  match t with
  | Base -> t
  (* | Var { index = _ ; value = Unknown } -> t *)
  | Unknown -> t
  | Var { index = _ ; value = t } ->
    shorten t
  | Carrier (s,c) -> Carrier(s,shorten c)
  | On (c,s) -> On(shorten c,s)
  | Onnot (c,s) -> Onnot(shorten c,s)
  | Arrow (cin,cout) -> Arrow(shorten cin, shorten cout)
  | CTuple cks -> CTuple (List.map shorten cks)
  (* | Unknown -> failwith "shorten" *)



open Carriers
let rec generalise_carrier c =
  let rec aux_car c =
    match c with
    | NameCar s -> VarCar (new_varcar ())
    | VarCar { cindex = n ; cvalue = t } -> aux_car t
    | _ -> c
  in
  aux_car c


let rec generalise tau =
  let car = VarCar (new_varcar ()) in
  let v = Var (new_varclock ()) in
  let rec aux t =
    match t with
    | Base -> v
     | Unknown -> v
     | Var { index = _ ; value = t } -> aux t
     | Arrow(t1,t2) -> Arrow(aux t1, aux t2)
     | CTuple ct -> CTuple (List.map aux ct)
     | On (c,s) -> On(aux c,s)
     | Onnot (c,s) -> Onnot(aux c,s)
     | Carrier (s,c) -> Carrier(s, aux c)
  in
  aux tau


(* All the unknown vars of tau (except the ones in gamma) get bounded  *)
let generalise_type gamma tau =
  let gamma = List.map (fun (a,b) -> b) gamma in
  (* let tau = generalise (full_shorten tau) in *)
  (* let tau = generalise_carriers tau in *)
  let genvars = (substract (vars_of_clock tau) (unknowns_of_type_env gamma)) in
  let gencars = carriers_of_clock tau in
  (* let genvars = make_set genvars in *)
  let gencars = make_set gencars in
  Forall(genvars,gencars,tau)


(* Printing variables  *)
let cvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

let rec print_clock fmt (t,v) =
    (* let names = *)
    (* let rec names_of = function *)
  (* | (n,[]) -> [] *)
  (* | (n, (_::xs)) -> (cvar_name n)::(names_of (n+1,xs)) *)
  (* in *)
  (* names_of (1,v) in *)
  (* let cvar_names = List.combine (List.rev v) names in *)
  match t with
  | Base -> Format.fprintf fmt "base"
  | Unknown ->
    Format.fprintf fmt "?"
  | Var { index = n ; value = Unknown } ->
    (* let name = *)
      (* try *)
        (* List.assoc n cvar_names *)
      (* with Not_found -> string_of_int n in *)
     Format.fprintf fmt "%d" n
  | Var { index = m ; value = t } ->
    Format.fprintf fmt "%a" print_clock (t,v)
  | Arrow(t1,t2) ->
    Format.fprintf fmt "( %a -> %a )" print_clock (t1,v) print_clock (t2,v)
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
    (* | Unknown -> Format.fprintf fmt  "?" *)


(* Shorten variables (follow indirection) *)
let rec shorten t =
  match t with
  | Var { index = _ ; value = Unknown } -> t
  (* | Unknown -> t *)
  | Var { index = _ ; value = (Var { index = m ; value = Unknown}) as tv } ->
    tv
  | Var ({ index = _ ; value = Var tv1} as tv2) ->
    tv2.value <- tv1.value;
    shorten t
  | Var { index = _ ; value = t' } -> shorten t'
  | Carrier (s,c) -> Carrier(s,shorten c)
  | On (c,s) -> On(shorten c,s)
  | Onnot (c,s) -> Onnot(shorten c,s)
  | Arrow(c1,c2) -> Arrow(shorten c1, shorten c2)
  | Unknown -> failwith "shorten"

  | _ ->
    t


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
    | Base -> Format.fprintf fmt "base"
    | Var { index = n ; value = Unknown } ->
      let name =
        (try List.assoc n cvar_names
         with  Not_found -> string_of_int n)
      in
      Format.fprintf fmt "%s" name
    | Var { index = m ; value = t } ->
      Format.fprintf fmt "*%a" print_rec t
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
  (* let rec print_list fmt l = *)
    (* match l with *)
    (* | [] -> Format.fprintf fmt "" *)
    (* | [x] -> Format.fprintf fmt "%s" *)
               (* (List.assoc x cvar_names) *)
    (* | x::xs -> Format.fprintf fmt "%s,%a" *)
                 (* (List.assoc x cvar_names) *)
                 (* print_list xs *)
  (* in *)
  (* Format.fprintf fmt "forall %a. %a" print_list (List.rev gv)  print_rec t *)
  Format.fprintf fmt "%a" print_rec t

(* Unification function : unifies types tau1 and tau2 *)
exception ClockClash of clock * clock

(* Check if a variable occurs in a type  *)
let occurs { index = n ; value = _ } t =
  let rec occrec c =
    let c = shorten c in
    match c with
    | Base -> false
    | Var { index = m ; value = _} -> (n = m)
    | Arrow (t1,t2) -> (occrec t1) || (occrec t2)
    | CTuple ts -> List.fold_left (fun acc t -> acc || occrec t) false ts
    | On (c,s) -> occrec c
    | Onnot (c,s) -> occrec c
    | Carrier (s,c') -> occrec c'
    | Unknown -> failwith "occurs"
  in occrec t

let rec unify_with_carriers (tau1,tau2) =
  let unify = unify_with_carriers in
  let tau1 = shorten tau1 in
  let tau2 = shorten tau2 in
  (* Format.fprintf Format.std_formatter "Unifying (with carriers) %a and %a \n%!" *)
  (* print_clock (tau1,[]) *)
  (* print_clock (tau2,[]); *)
  begin
    match tau1, tau2 with
    | Base, Base -> ()
    | unknown , Unknown -> ()
    | Carrier (s,c) , Carrier (s',d) ->
      unify_carriers(s,s');
      unify(c,d);
    | Var ({ index = n; value = Unknown} as tv1),
      Var { index = m; value = Unknown} ->
      if n <> m then tv1.value <- tau2;
    | Carrier(s,c) , Var ({index = _ ; value = Unknown} as tv) ->
      if not (occurs tv tau1) then tv.value <- tau1
      else (raise (ClockClash (tau1,tau2)))
    | _ , Var ({index = _ ; value = Unknown} as tv) ->
      if not (occurs tv tau1) then tv.value <- tau1
      else (raise (ClockClash (tau1,tau2)))
    | Var ({index = m ; value = Unknown} as tv), Carrier(s,c) ->
      (* unify (tau1, c) *)
      if not (occurs tv tau2) then
        tv.value <- tau2
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
    (* | On (c,x), Carrier(s,d) -> *)
      (* unify(c,tau2); *)
    | On (c,x) , On (d,y) ->
      unify_carriers(x,y);
      unify(c,d);
    | Carrier (s,c) , _ -> unify(c,tau2)
    | On(c,x) , Carrier (s,d) ->
      unify(d,tau1)
     | _ -> raise (ClockClash (tau1,tau2))
  end
   (* ;Format.fprintf Format.std_formatter "After unifying : %a and %a \n" *)
  (* print_clock (tau1,[]) *)
  (* print_clock(tau2,[]) *)

let rec unify (tau1,tau2) =
  let tau1 = shorten tau1 in
  let tau2 = shorten tau2 in
  (* Format.printf  "Unifying %a and %a \n%!" *)
  (* print_clock (tau1,[]) *)
  (* print_clock (tau2,[]); *)
  begin
    match tau1, tau2 with
    | Base, Base -> ()
    | Unknown , Unknown -> ()
    | Carrier (s,c) , Carrier (s',d) ->
      unify_carriers(s,s');
      unify(c,d);
    | Carrier (s,c) , _ -> unify(c,tau2)
     | Var ({index = n; value= Unknown} as tv), Carrier (s,c) ->
    (* | _, Carrier (s,c) -> *)
      if not (occurs tv tau2) then
      tv.value <- Carrier(s,c)
      (* unify(tau1,c) *)
    | Var ({ index = n; value = Unknown} as tv1),
      Var { index = m; value = Unknown} ->
      if n <> m then tv1.value <- tau2;
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
      begin
      try
      unify_carriers(x,y);
      unify(c,d);
      with _ -> raise (ClockClash (tau1,tau2))
    end
      (* | Carrier (s,c) , _ -> unify(c,tau2) *)
    (* | _ , Carrier (s,c) -> unify(tau1,c) *)
    | _ -> raise (ClockClash (tau1,tau2))
  end
  (* ;Format.fprintf Format.std_formatter "After unifying : %a and %a \n" *)
  (* print_clock (tau1,[]) *)
  (* print_clock(tau2,[]) *)


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
  let cunknowns = List.map (fun n -> n, VarCar(new_varcar ())) gc in
  let unknowns = List.map (fun n -> n, Var(new_varclock ())) gv in
  let rec ginstance t =
    match t with
    | Base -> Base
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
    | On (c,s) -> On (ginstance c,carinstance s cunknowns)
    | Onnot (c,s) -> Onnot (ginstance c,carinstance s cunknowns)
    | Carrier (s,c) -> Carrier (carinstance s cunknowns,ginstance c)
    | Unknown -> failwith "gen_instance"
  in
  ginstance tau

let clock_lookup (n:string) (gamma: env_elem list) =
    try
    List.assoc n gamma
    with Not_found ->
      Forall([],[],Var(new_varclock()))
    (* let s = Format.asprintf "Not found : %s" n in *)
    (* Error.print_error Location.none s *)

let print_env fmt (env : env_elem list) =
  let rec print_list fmt l =
    match l with
      [] -> Format.fprintf fmt ""
    | [(p,s)] -> Format.fprintf fmt "\t(%s:%a)"
                   p
                   print_clock_scheme s
    | (p,s)::xs -> Format.fprintf fmt "\t(%s:%a)\n%a"
                 p
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


let rec string_of_pat p =
  match p.p_desc with
  | Ident i -> i
  | Typed(p,t) -> string_of_pat p
  | PUnit -> "()"
  | Tuple t -> failwith "sop"

    (* List.fold_left (fun acc x -> string_of_pat x^acc) "" t *)

(* Function for finding clock in an env *)
let rec lookup_clock env p =
  let s = string_of_pat p in
  try
    List.assoc s env
  with Not_found ->
    Error.print_error p.p_loc ("Unbound variable "^s)


(* let _ =
 *   let a = Var(new_varclock ()) in
 *   let b = Var(new_varclock ()) in
 *   let c = VarCar(new_varcar ()) in
 *   let a = On(a,c) in
 *   let d = Carrier(VarCar(new_varcar ()), b) in
 *   Format.printf "Before : %a and %a \n" print_clock (a,[])  print_clock (d,[]);
 *   unify(a,d);
 *   Format.printf "After : %a and %a \n" print_clock (a,[])  print_clock (d,[]); *)
