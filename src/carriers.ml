open Clocking_ast

exception CarrierClash of carrier * carrier
let new_varcar, reset_varcar =
  let cpt = ref 0 in
  (fun () ->
    incr cpt;
    { cindex = !cpt ; cvalue = UnknownCar}
  ),
  fun () -> cpt := 0

let rec string_of_carrier c =
  match c with
  | NameCar s -> s
  | UnknownCar -> "ukn"
    (* failwith "string_of_carrier" *)
  | VarCar { cindex = n; cvalue = t } ->
    string_of_carrier t

(* Harvest all the carriers in tau  *)
let carriers_of_clock tau =
  let rec cars vs t =
    match t with
    | NameCar s -> vs
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

(* Printing carriers *)
let car_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (64+r)) in
    if q = 0 then s else (name_of q)^s
  in "ck"^(name_of n)

let rec print_carrier fmt c =
  match c with
  | NameCar s -> Format.fprintf fmt "%s" s
  | UnknownCar -> Format.fprintf fmt "?"
  | VarCar { cindex = n; cvalue = UnknownCar} -> Format.fprintf fmt "%s"  (car_name n)
  | VarCar { cindex = n; cvalue = c} -> Format.fprintf fmt "%a" print_carrier c

(* Shorten variables (follow indirection) *)
let rec car_shorten c =
  match c with
  | VarCar { cindex = _ ; cvalue = UnknownCar } -> c
  | VarCar { cindex = _ ; cvalue = (VarCar { cindex = m ; cvalue = UnknownCar}) as tv } ->
    tv
  | VarCar ({ cindex = _ ; cvalue = VarCar tv1} as tv2) ->
    tv2.cvalue <- tv1.cvalue; car_shorten c
  | UnknownCar -> failwith "shorten"
  | _ -> c

let caroccurs { cindex = n ; cvalue = _ } c =
  let rec occrec c =
    Format.fprintf Format.std_formatter "Occurs %d in %a ? \n%!"
      n
      print_carrier c;
    let c = car_shorten c in
    match c with
    | NameCar s -> false
    | VarCar { cindex = m ; cvalue = _} -> (n = m)
    | UnknownCar -> failwith "occurs"
  in occrec c

let carinstance c cunknowns =
    let rec aux c =
      match c with
      | NameCar s -> NameCar s
      | VarCar { cindex = n; cvalue = UnknownCar } ->
        (try List.assoc n cunknowns with Not_found -> c)
      | VarCar { cindex = _ ; cvalue = t } -> aux t
      | UnknownCar -> failwith "car instance"
    in
    aux c

let unify_carriers (c1,c2) =
  let c1,c2 = car_shorten c1, car_shorten c2 in
   (* Format.fprintf Format.std_formatter "Unifying carriers %a and %a \n%!" *)
  (* print_carrier c1 *)
  (* print_carrier c2; *)
  match c1,c2 with
  | NameCar x, NameCar y -> if not (x = y) then raise (CarrierClash (c1,c2)) else ()
  | VarCar ({ cindex = n; cvalue = UnknownCar } as cv1), VarCar { cindex = m ; cvalue = UnknownCar } ->
    if n <> m then cv1.cvalue <- c2
  | _ , VarCar ({cindex = _; cvalue = UnknownCar} as cv) ->
    if not (caroccurs cv c1) then cv.cvalue <- c1
    else failwith "caroccurs"
  | VarCar ({cindex = _; cvalue = UnknownCar} as cv), _ ->
    if not (caroccurs cv c2) then cv.cvalue <- c2
    else failwith "carrocurs"
  | _ -> failwith "unify_carriers"
