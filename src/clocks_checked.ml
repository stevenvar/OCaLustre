
(** val string_dec : char list -> char list -> bool **)

let rec string_dec s x =
  match s with
  | [] -> (match x with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x with
     | [] -> false
     | a0::s1 -> if (=) a a0 then string_dec s0 s1 else false)

type 'a nelist =
| Nebase of 'a
| Necons of 'a * 'a nelist

type ident = char list

type const =
| Cc

type operator =
| Op

type clock =
| Cbase
| Con of clock * ident
| Conot of clock * ident

type lexp =
| Econst of const * clock
| Evar of ident
| Ebinop of operator * lexp * lexp
| Ewhen of lexp * ident
| Ewhenot of lexp * ident

type lidents = ident nelist

type lexps = lexp nelist

type cexp =
| Eexp of lexp
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp

type equation =
| EqDef of ident * clock * cexp
| EqFby of ident * clock * const * lexp
| EqApp of lidents * clock * ident * lexps

type leqns = equation nelist

type d =
| Mk_node of ident * lidents * lidents * leqns

(** val ident_eqb : char list -> char list -> bool **)

let ident_eqb s1 s2 =
  if string_dec s1 s2 then true else false

type clockenv = (char list*clock) list

(** val assoc : char list -> clockenv -> clock option **)

let rec assoc x = function
| [] -> None
| p :: l0 -> let y,c = p in if ident_eqb x y then Some c else assoc x l0

(** val clock_eqb : clock -> clock -> bool **)

let rec clock_eqb c1 c2 =
  match c1 with
  | Cbase -> (match c2 with
              | Cbase -> true
              | _ -> false)
  | Con (x, c) ->
    (match c2 with
     | Con (y, d0) -> (&&) (clock_eqb x y) (ident_eqb c d0)
     | _ -> false)
  | Conot (x, c) ->
    (match c2 with
     | Conot (y, d0) -> (&&) (clock_eqb x y) (ident_eqb c d0)
     | _ -> false)

(** val clockof_var : clockenv -> ident -> clock option **)

let rec clockof_var c x =
  assoc x c

(** val clockof_vars : clockenv -> ident nelist -> clock option **)

let rec clockof_vars c = function
| Nebase h -> clockof_var c h
| Necons (h, t) ->
  let ck = clockof_var c h in
  let ckt = clockof_vars c t in
  (match ck with
   | Some ck0 ->
     (match ckt with
      | Some ckt0 -> if clock_eqb ck0 ckt0 then Some ck0 else None
      | None -> None)
   | None -> None)

(** val clockof_lexp : clockenv -> lexp -> clock option **)

let rec clockof_lexp c = function
| Econst (_, ck) -> Some ck
| Evar x -> assoc x c
| Ebinop (_, e1, e2) ->
  let c1 = clockof_lexp c e1 in
  let c2 = clockof_lexp c e2 in
  (match c1 with
   | Some a ->
     (match c2 with
      | Some b -> if clock_eqb a b then Some a else None
      | None -> None)
   | None -> None)
| Ewhen (e, c0) ->
  let c1 = clockof_lexp c e in
  let c2 = assoc c0 c in
  (match c1 with
   | Some a ->
     (match c2 with
      | Some b -> if clock_eqb a b then Some (Con (a, c0)) else None
      | None -> None)
   | None -> None)
| Ewhenot (e, c0) ->
  let c1 = clockof_lexp c e in
  let c2 = assoc c0 c in
  (match c1 with
   | Some a ->
     (match c2 with
      | Some b -> if clock_eqb a b then Some (Conot (a, c0)) else None
      | None -> None)
   | None -> None)

(** val clockof_lexps : clockenv -> lexp nelist -> clock option **)

let rec clockof_lexps c = function
| Nebase h -> clockof_lexp c h
| Necons (h, t) ->
  let ck = clockof_lexp c h in
  let ckt = clockof_lexps c t in
  (match ck with
   | Some ck0 ->
     (match ckt with
      | Some ckt0 -> if clock_eqb ck0 ckt0 then Some ck0 else None
      | None -> None)
   | None -> None)

(** val clockof_cexp : clockenv -> cexp -> clock option **)

let rec clockof_cexp c = function
| Eexp e -> clockof_lexp c e
| Emerge (x, e1, e2) ->
  let cx = assoc x c in
  let c1 = clockof_cexp c e1 in
  let c2 = clockof_cexp c e2 in
  (match cx with
   | Some ck ->
     (match c1 with
      | Some c0 ->
        (match c0 with
         | Con (u, n) ->
           (match c2 with
            | Some c3 ->
              (match c3 with
               | Conot (v, m) ->
                 if (&&) (ident_eqb x n)
                      ((&&) (clock_eqb ck u)
                        ((&&) (clock_eqb u v) (ident_eqb n m)))
                 then Some ck
                 else None
               | _ -> None)
            | None -> None)
         | _ -> None)
      | None -> None)
   | None -> None)
| Eif (e, t, f) ->
  let ce = clockof_lexp c e in
  let ct = clockof_cexp c t in
  let cf = clockof_cexp c f in
  (match ce with
   | Some a ->
     (match ct with
      | Some b ->
        (match cf with
         | Some c0 ->
           if (&&) (clock_eqb a b) (clock_eqb b c0) then Some c0 else None
         | None -> None)
      | None -> None)
   | None -> None)

(** val well_clocked_eq : clockenv -> equation -> bool **)

let rec well_clocked_eq c = function
| EqDef (i, ck, ce) ->
  (match clockof_var c i with
   | Some c0 ->
     (match clockof_cexp c ce with
      | Some d0 -> (&&) (clock_eqb c0 d0) (clock_eqb c0 ck)
      | None -> false)
   | None -> false)
| EqFby (i, ck, _, e) ->
  (match clockof_var c i with
   | Some d0 ->
     (match clockof_lexp c e with
      | Some c0 -> (&&) (clock_eqb d0 c0) (clock_eqb c0 ck)
      | None -> false)
   | None -> false)
| EqApp (ii, ck, _, es) ->
  let ck' = clockof_lexps c es in
  (match clockof_vars c ii with
   | Some d0 ->
     (match ck' with
      | Some ck'0 -> (&&) (clock_eqb d0 ck'0) (clock_eqb ck ck'0)
      | None -> false)
   | None -> false)

(** val well_clocked_eqs : clockenv -> equation nelist -> bool **)

let rec well_clocked_eqs c = function
| Nebase eq -> well_clocked_eq c eq
| Necons (e, eqs0) -> (&&) (well_clocked_eq c e) (well_clocked_eqs c eqs0)

(** val well_clocked_node : clockenv -> d -> bool **)

let rec well_clocked_node c = function
| Mk_node (_, xs, ys, eqns) ->
  let chk = well_clocked_eqs c eqns in
  let in_clocks = clockof_vars c xs in
  let out_clocks = clockof_vars c ys in
  (match in_clocks with
   | Some c0 ->
     (match c0 with
      | Cbase ->
        (match out_clocks with
         | Some c1 -> (match c1 with
                       | Cbase -> chk
                       | _ -> false)
         | None -> false)
      | _ -> false)
   | None -> false)
