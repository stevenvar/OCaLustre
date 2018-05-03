
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

type ident = char list

(** val ident_eqb : char list -> char list -> bool **)

let ident_eqb s1 s2 =
  if string_dec s1 s2 then true else false

type clock =
| Cbase
| Con of clock * ident * bool

type const =
| Cint of int
| Cbool of bool

type operator =
| Plus
| Minus
| Times

type lexp =
| Econst of const
| Evar of ident
| Ewhen of lexp * ident * bool
| Ebinop of operator * lexp * lexp

type lexps = lexp list

type cexp =
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp
| Eexp of lexp

type equation =
| EqDef of ident * clock * cexp
| EqApp of ident * clock * ident * lexps
| EqFby of ident * clock * const * lexp

type node = { n_name : ident; n_input : ident list; n_output : ident list;
              n_eqs : equation list }

(** val n_input : node -> ident list **)

let n_input x = x.n_input

(** val n_output : node -> ident list **)

let n_output x = x.n_output

(** val n_eqs : node -> equation list **)

let n_eqs x = x.n_eqs

type clockenv = (char list*clock) list

(** val assoc : char list -> clockenv -> clock option **)

let rec assoc x = function
| [] -> None
| p :: l0 -> let y,c = p in if ident_eqb x y then Some c else assoc x l0

(** val eqb : bool -> bool -> bool **)

let rec eqb b1 b2 =
  if b1 then b2 else if b2 then false else true

(** val clock_eqb : clock -> clock -> bool **)

let rec clock_eqb c1 c2 =
  match c1 with
  | Cbase -> (match c2 with
              | Cbase -> true
              | Con (_, _, _) -> false)
  | Con (x, c, b) ->
    (match c2 with
     | Cbase -> false
     | Con (y, d, k) -> (&&) ((&&) (clock_eqb x y) (ident_eqb c d)) (eqb b k))

(** val clockof_var : clockenv -> ident -> clock option **)

let rec clockof_var c x =
  assoc x c

(** val clockof_vars : clockenv -> ident list -> clock option **)

let rec clockof_vars c = function
| [] -> None
| h :: t ->
  (match t with
   | [] -> clockof_var c h
   | _ :: _ ->
     let ck = clockof_var c h in
     let ckt = clockof_vars c t in
     (match ck with
      | Some ck0 ->
        (match ckt with
         | Some ckt0 -> if clock_eqb ck0 ckt0 then Some ck0 else None
         | None -> None)
      | None -> None))

(** val clockof_lexp : clockenv -> lexp -> clock option **)

let rec clockof_lexp c = function
| Econst _ -> Some Cbase
| Evar x -> assoc x c
| Ewhen (e, c0, k) ->
  let c1 = clockof_lexp c e in
  let c2 = assoc c0 c in
  (match c1 with
   | Some a ->
     (match c2 with
      | Some b -> if clock_eqb a b then Some (Con (a, c0, k)) else None
      | None -> None)
   | None -> None)
| Ebinop (_, e1, e2) ->
  let c1 = clockof_lexp c e1 in
  let c2 = clockof_lexp c e2 in
  (match c1 with
   | Some a ->
     (match c2 with
      | Some b -> if clock_eqb a b then Some a else None
      | None -> None)
   | None -> None)

(** val clockof_lexps : clockenv -> lexp list -> clock option **)

let rec clockof_lexps c = function
| [] -> None
| h :: t ->
  (match t with
   | [] -> clockof_lexp c h
   | _ :: _ ->
     let ck = clockof_lexp c h in
     let ckt = clockof_lexps c t in
     (match ck with
      | Some ck0 ->
        (match ckt with
         | Some ckt0 -> if clock_eqb ck0 ckt0 then Some ck0 else None
         | None -> None)
      | None -> None))

(** val clockof_cexp : clockenv -> cexp -> clock option **)

let rec clockof_cexp c = function
| Emerge (x, e1, e2) ->
  let cx = assoc x c in
  let c1 = clockof_cexp c e1 in
  let c2 = clockof_cexp c e2 in
  (match cx with
   | Some ck ->
     (match c1 with
      | Some y ->
        (match y with
         | Cbase -> None
         | Con (u, n, b) ->
           if b
           then (match c2 with
                 | Some c0 ->
                   (match c0 with
                    | Cbase -> None
                    | Con (v, m, b0) ->
                      if b0
                      then None
                      else if (&&) (ident_eqb x n)
                                ((&&) (clock_eqb ck u)
                                  ((&&) (clock_eqb u v) (ident_eqb n m)))
                           then Some ck
                           else None)
                 | None -> None)
           else None)
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
| Eexp e -> clockof_lexp c e

(** val well_clocked_eq : clockenv -> equation -> bool **)

let rec well_clocked_eq c = function
| EqDef (i, ck, ce) ->
  (match clockof_var c i with
   | Some c0 ->
     (match clockof_cexp c ce with
      | Some d -> (&&) (clock_eqb c0 d) (clock_eqb c0 ck)
      | None -> false)
   | None -> false)
| EqApp (i, ck, _, es) ->
  let ck' = clockof_lexps c es in
  (match clockof_var c i with
   | Some d ->
     (match ck' with
      | Some ck'0 -> (&&) (clock_eqb d ck'0) (clock_eqb ck ck'0)
      | None -> false)
   | None -> false)
| EqFby (i, ck, _, e) ->
  (match clockof_var c i with
   | Some d ->
     (match clockof_lexp c e with
      | Some c0 -> (&&) (clock_eqb d c0) (clock_eqb c0 ck)
      | None -> false)
   | None -> false)

(** val well_clocked_eqs : clockenv -> equation list -> bool **)

let rec well_clocked_eqs c = function
| [] -> true
| e :: eqs0 ->
  (match eqs0 with
   | [] -> well_clocked_eq c e
   | _ :: _ -> (&&) (well_clocked_eq c e) (well_clocked_eqs c eqs0))

(** val well_clocked_node : clockenv -> node -> bool **)

let rec well_clocked_node c n =
  let chk = well_clocked_eqs c n.n_eqs in
  let in_clocks = clockof_vars c n.n_input in
  let out_clocks = clockof_vars c n.n_output in
  (match in_clocks with
   | Some c0 ->
     (match c0 with
      | Cbase ->
        (match out_clocks with
         | Some c1 -> (match c1 with
                       | Cbase -> chk
                       | Con (_, _, _) -> false)
         | None -> false)
      | Con (_, _, _) -> false)
   | None -> false)
