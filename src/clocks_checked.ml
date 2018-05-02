
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true



(** val fold_left : ('a1 -> 'a2 -> 'a1) -> 'a2 list -> 'a1 -> 'a1 **)

let rec fold_left f l a0 =
  match l with
  | [] -> a0
  | b :: t -> fold_left f t (f a0 b)

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

(** val clock_option_eqb : clock option -> clock option -> bool **)

let rec clock_option_eqb c1 c2 =
  match c1 with
  | Some x -> (match c2 with
               | Some y -> clock_eqb x y
               | None -> false)
  | None -> (match c2 with
             | Some _ -> false
             | None -> true)

(** val clockof_var : clockenv -> ident -> clock option **)

let rec clockof_var c x =
  assoc x c

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

(** val clockof_equation : clockenv -> equation -> clock option **)

let clockof_equation c = function
| EqDef (_, ck, ce) ->
  (match clockof_cexp c ce with
   | Some c0 -> if clock_eqb c0 ck then Some ck else None
   | None -> None)
| EqApp (_, ck, _, es) ->
  let ck' = clockof_lexps c es in
  (match ck' with
   | Some ck'0 -> if clock_eqb ck ck'0 then Some ck else None
   | None -> None)
| EqFby (_, ck, _, e0) ->
  (match clockof_lexp c e0 with
   | Some c0 -> if clock_eqb c0 ck then Some ck else None
   | None -> None)

(** val clockof_eqs : clockenv -> equation list -> clock option **)

let rec clockof_eqs c = function
| [] -> None
| e :: eqs0 ->
  (match eqs0 with
   | [] -> clockof_equation c e
   | _ :: _ ->
     if clock_option_eqb (clockof_equation c e) None
     then None
     else clockof_eqs c eqs0)

(** val clockof_node : clockenv -> node -> clock option **)

let clockof_node c n =
  let chk = clockof_eqs c n.n_eqs in
  let in_all_base =
    fold_left (fun acc x ->
      if clock_option_eqb acc (clockof_var c x) then acc else None) n.n_input
      (Some Cbase)
  in
  let out_all_base =
    fold_left (fun acc x ->
      if clock_option_eqb acc (clockof_var c x) then acc else None)
      n.n_output (Some Cbase)
  in
  if (&&) (negb (clock_option_eqb chk None))
       (clock_option_eqb in_all_base out_all_base)
  then in_all_base
  else None
