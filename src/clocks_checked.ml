


(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

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

type operator =
| Op

type clock =
| Cbase
| Con of clock * ident
| Conot of clock * ident

type const =
| Cc

type lexp =
| Econst of const * clock
| Evar of ident
| Ebinop of operator * lexp * lexp
| Ewhen of lexp * ident
| Ewhenot of lexp * ident

type cexp =
| Eexp of lexp
| Emerge of ident * cexp * cexp
| Eif of lexp * cexp * cexp

type lexps = lexp nelist

type lidents = ident nelist

type lclocks = clock nelist

type equation =
| EqDef of ident * clock * cexp
| EqFby of ident * clock * const * lexp
| EqApp of lidents * clock * ident * lexps
| EqTuple of lidents * lclocks * lexps

type leqns = equation nelist

type tau = clock nelist*ident nelist

type d =
| Mk_node of ident * lidents * lclocks * lidents * lclocks * leqns

type ident_pair = ident*ident

type xlist = ident list

type lident_pair = ident_pair list



let string_of_char_list cl =
  let s = Bytes.make (List.length cl) ' ' in
  let rec loop cl i =
    match cl with
    | [] -> s
    | c::cl -> Bytes.set s i c; loop cl (i+1)
  in
  let b = loop cl 0 in
  Bytes.to_string b

let rec print_clock fmt c =
  match c with
  | Cbase -> Format.fprintf fmt "Cbase"
  | Con (c,s) -> Format.printf "%a on %s" print_clock c (string_of_char_list s)
  | Conot (c,s) -> Format.printf "%a on not %s" print_clock c (string_of_char_list s)

let rec iter_nelist f l =
  match l with
  | Nebase x -> f x
  | Necons (x,l) -> f x; iter_nelist f l

(** val subst_ck : clock -> clock -> clock **)

let rec subst_ck x1 x2 =
  match x1 with
  | Cbase -> x2
  | Con (ck, x) -> Con ((subst_ck ck x2), x)
  | Conot (ck, x) -> Conot ((subst_ck ck x2), x)

(** val subst_base : lclocks -> clock -> lclocks **)

let rec subst_base x1 x2 =
  match x1 with
  | Nebase ck -> Nebase (subst_ck ck x2)
  | Necons (ck, cks) -> Necons ((subst_ck ck x2), (subst_base cks x2))

(** val clockof : clock -> xlist **)

let rec clockof = function
| Cbase -> []
| Con (ck, x) -> x :: (clockof ck)
| Conot (ck, x) -> x :: (clockof ck)

(** val clocks : lclocks -> xlist **)

let rec clocks = function
| Nebase ck -> clockof ck
| Necons (ck, cks) -> app (clockof ck) (clocks cks)

(** val ident_eqb : char list -> char list -> bool **)

let ident_eqb s1 s2 =
  if string_dec s1 s2 then true else false

(** val assoc_sub : char list -> (char list*char list) list -> char list **)

let rec assoc_sub x = function
| [] -> x
| p :: l0 -> let y,c = p in if ident_eqb x y then c else assoc_sub x l0

(** val subst_name_ck : clock -> lident_pair -> clock **)

let rec subst_name_ck x1 x2 =
  match x1 with
  | Cbase -> Cbase
  | Con (ck, x) -> Con ((subst_name_ck ck x2), (assoc_sub x x2))
  | Conot (ck, x) -> Conot ((subst_name_ck ck x2), (assoc_sub x x2))

(** val subst_names : clock nelist -> lident_pair -> clock nelist **)

let rec subst_names cks sigma =
  match cks with
  | Nebase c -> Nebase (subst_name_ck c sigma)
  | Necons (c, cks0) ->
    Necons ((subst_name_ck c sigma), (subst_names cks0 sigma))

type clockenv = (char list*clock) list

type globalclockenv = (char list*(tau*tau)) list

(** val assoc : char list -> clockenv -> clock option **)

let rec assoc x = function
| [] -> None
| p :: l0 -> let y,c = p in if ident_eqb x y then Some c else assoc x l0

(** val assoc_global : char list -> globalclockenv -> (tau*tau) option **)

let rec assoc_global x = function
| [] -> None
| p :: l0 ->
  let y,c = p in if ident_eqb x y then Some c else assoc_global x l0

(** val mem_S : char list -> xlist -> bool **)

let rec mem_S x = function
| [] -> false
| y :: l0 -> if ident_eqb x y then true else mem_S x l0

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

(** val clocks_eqb : clock nelist -> clock nelist -> bool **)

let rec clocks_eqb cl1 cl2 =
  match cl1 with
  | Nebase c ->
    (match cl2 with
     | Nebase d0 -> clock_eqb c d0
     | Necons (_, _) -> false)
  | Necons (c, cs) ->
    (match cl2 with
     | Nebase _ -> false
     | Necons (d0, ds) -> (&&) (clock_eqb c d0) (clocks_eqb cs ds))

(** val clk_subst_fun :
    char list nelist -> lexp nelist -> xlist -> (char list*ident) list option **)

let rec clk_subst_fun xs es s =
  match xs with
  | Nebase x ->
    (match es with
     | Nebase e ->
       (match e with
        | Evar y -> if mem_S x s then Some ((x,y) :: []) else Some []
        | _ -> if mem_S x s then None else Some [])
     | Necons (_, _) -> None)
  | Necons (x, xs0) ->
    (match es with
     | Nebase _ -> None
     | Necons (e, es0) ->
       (match e with
        | Evar y ->
          if mem_S x s
          then (match clk_subst_fun xs0 es0 s with
                | Some sigma -> Some ((x,y) :: sigma)
                | None -> None)
          else clk_subst_fun xs0 es0 s
        | _ -> if mem_S x s then None else clk_subst_fun xs0 es0 s))

(** val clk_subst_var_fun :
    lidents -> lidents -> xlist -> (ident*ident) list option **)

let rec clk_subst_var_fun xs xs' s =
  match xs with
  | Nebase x ->
    (match xs' with
     | Nebase y -> if mem_S x s then Some ((x,y) :: []) else Some []
     | Necons (_, _) -> None)
  | Necons (x, xs0) ->
    (match xs' with
     | Nebase _ -> None
     | Necons (y, ys) ->
       if mem_S x s
       then (match clk_subst_var_fun xs0 ys s with
             | Some sigma -> Some ((x,y) :: sigma)
             | None -> None)
       else clk_subst_var_fun xs0 ys s)

(** val clockof_var : clockenv -> ident -> clock option **)

let rec clockof_var c x =
  assoc x c

(** val clockof_global_var : globalclockenv -> ident -> (tau*tau) option **)

let rec clockof_global_var c x =
  assoc_global x c

(** val clockof_vars : clockenv -> ident nelist -> clock nelist option **)

let rec clockof_vars c = function
| Nebase h ->
  (match clockof_var c h with
   | Some c0 -> Some (Nebase c0)
   | None -> None)
| Necons (h, t) ->
  let ck = clockof_var c h in
  let ckt = clockof_vars c t in
  (match ck with
   | Some ck0 ->
     (match ckt with
      | Some ckt0 -> Some (Necons (ck0, ckt0))
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

(** val clockof_lexps : clockenv -> lexp nelist -> clock nelist option **)

let rec clockof_lexps c = function
| Nebase h ->
  (match clockof_lexp c h with
   | Some c0 -> Some (Nebase c0)
   | None -> None)
| Necons (h, t) ->
  let ck = clockof_lexp c h in
  let ckt = clockof_lexps c t in
  (match ck with
   | Some ck0 ->
     (match ckt with
      | Some ckt0 -> Some (Necons (ck0, ckt0))
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

(** val well_clocked_eq : globalclockenv -> clockenv -> equation -> bool **)

let rec well_clocked_eq g c = function
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
| EqApp (xs, ck, f, es) ->
   (* Format.fprintf Format.std_formatter "ck=%a\n" print_clock ck; *)
  let fck = clockof_global_var g f in
  (match fck with
   | Some p ->
     let t,t0 = p in
     let cks1,xs1 = t in
     let cks2,xs2 = t0 in
     let cks3 = clockof_lexps c es in
     let cks4 = clockof_vars c xs in
     (match cks3 with
      | Some cks5 ->
        (match cks4 with
         | Some cks6 ->
           let s = app (clocks cks1) (clocks cks2) in
           let sigma = clk_subst_fun xs1 es s in
           let sigma' = clk_subst_var_fun xs2 xs s in
           (match sigma with
            | Some sigma0 ->
              (match sigma' with
               | Some sigma'0 ->
                 (&&)
                   (clocks_eqb cks5 (subst_base (subst_names cks1 sigma0) ck))
                   (clocks_eqb cks6
                     (subst_base (subst_names cks2 (app sigma'0 sigma0)) ck))
               | None -> false)
            | None -> false)
         | None -> false)
      | None -> false)
   | None -> false)
| EqTuple (xs, cks, es) ->
  (match clockof_lexps c es with
   | Some ces ->
     (match clockof_vars c xs with
      | Some cxs -> (&&) (clocks_eqb ces cxs) (clocks_eqb ces cks)
      | None -> false)
   | None -> false)

(** val well_clocked_eqs :
    globalclockenv -> clockenv -> equation nelist -> bool **)

let rec well_clocked_eqs g c = function
  | Nebase eq ->
     let wc = well_clocked_eq g c eq in
     (* Format.fprintf Format.std_formatter "wc = %b" wc; *)
     wc
| Necons (e, eqs0) ->
   let wc = (well_clocked_eq g c e) in
   let wcs = (well_clocked_eqs g c eqs0) in
   (* Format.fprintf Format.std_formatter "wc = %b / wcs = %b" wc wcs; *)
   wc && wcs




(** val well_clocked_node : globalclockenv -> clockenv -> d -> bool **)

let rec well_clocked_node g c = function
  | Mk_node (_, xs, cks, ys, cks', eqns) ->
    let chk = well_clocked_eqs g c eqns in
    let in_clocks = clockof_vars c xs in
    let out_clocks = clockof_vars c ys in
    (match in_clocks with
     | Some c0 ->
       (match out_clocks with
        | Some d0 ->
           (* iter_nelist (fun s -> print_clock Format.std_formatter s) c0; *)
           (* iter_nelist (fun s -> print_clock Format.std_formatter s) cks; *)
           (* iter_nelist (fun s -> print_clock Format.std_formatter s) d0; *)
           (* iter_nelist (fun s -> print_clock Format.std_formatter s) cks'; *)
           (* Format.fprintf Format.std_formatter "%b" chk; *)
           (&&) ((&&) (clocks_eqb cks c0) (clocks_eqb cks' d0)) chk
        | None -> false)
     | None -> false)
