
(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val eq_bool : bool -> bool -> bool **)

let eq_bool x y =
  if x then if y then true else false else if y then false else true

(** val eq_ascii : char -> char -> bool **)

let eq_ascii x y =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun x0 x1 x2 x3 x4 x5 x6 x7 ->
    (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
      (fun b7 b8 b9 b10 b11 b12 b13 b14 ->
      if eq_bool x0 b7
      then if eq_bool x1 b8
           then if eq_bool x2 b9
                then if eq_bool x3 b10
                     then if eq_bool x4 b11
                          then if eq_bool x5 b12
                               then if eq_bool x6 b13 then eq_bool x7 b14 else false
                               else false
                          else false
                     else false
                else false
           else false
      else false)
      y)
    x

type identifier = char list

(** val eq_identifier : identifier -> identifier -> bool **)

let rec eq_identifier s0 x0 =
  match s0 with
  | [] -> (match x0 with
           | [] -> true
           | _::_ -> false)
  | a::s1 ->
    (match x0 with
     | [] -> false
     | a0::s2 -> if eq_ascii a a0 then eq_identifier s1 s2 else false)

type type_constr = char list

type ocaml_expr = char list

(** val eq_ocaml_expr : ocaml_expr -> ocaml_expr -> bool **)

let rec eq_ocaml_expr s0 x0 =
  match s0 with
  | [] -> (match x0 with
           | [] -> true
           | _::_ -> false)
  | a::s1 ->
    (match x0 with
     | [] -> false
     | a0::s2 -> if eq_ascii a a0 then eq_ocaml_expr s1 s2 else false)

type unop =
| Unopminus
| Unopminusf
| Unopnot

type constant =
| Kint
| Kfloat
| Kbool

type binop =
| Binopop_int
| Binopop_float
| Binopop_compare
| Binopop_bool

type clock =
| Ckbase
| Ckon of clock * identifier
| Ckonnot of clock * identifier
| Cktuple of clock * clock
| Ckarrow of clock * clock

type lexp =
| Eunit of clock
| Econst of constant * clock
| Evar of identifier
| Econstructor of type_constr * clock
| Ebinop of lexp * binop * lexp
| Eunop of unop * lexp
| Ewhen of lexp * identifier
| Ewhennot of lexp * identifier

type cexp =
| Ceexp of lexp
| Cemerge of identifier * cexp * cexp
| Ceif of lexp * cexp * cexp

type pattern =
| Patp_unit
| Patp_var of identifier
| Patp_tuple of identifier * pattern

type lexps =
| Esone_exp of lexp
| Escons_exps of lexp * lexps

type equation =
| EqDef of pattern * clock * cexp
| EqFby of pattern * clock * constant * lexp
| EqApp of pattern * clock * identifier * lexps
| EqEval of pattern * clock * ocaml_expr

type leqns =
| Eqseqs_one of equation
| Eqseqs_cons of equation * leqns

type c = (char list*clock) list

type nodedef =
| Nodemk_node of identifier * pattern * pattern * leqns

(** val eq_clock : clock -> clock -> bool **)

let rec eq_clock c0 x0 =
  match c0 with
  | Ckbase -> (match x0 with
               | Ckbase -> true
               | _ -> false)
  | Ckon (ck, x) ->
    (match x0 with
     | Ckon (ck0, x1) -> if eq_clock ck ck0 then eq_ocaml_expr x x1 else false
     | _ -> false)
  | Ckonnot (ck, x) ->
    (match x0 with
     | Ckonnot (ck0, x1) -> if eq_clock ck ck0 then eq_ocaml_expr x x1 else false
     | _ -> false)
  | Cktuple (ck, ck') ->
    (match x0 with
     | Cktuple (ck0, ck'0) -> if eq_clock ck ck0 then eq_clock ck' ck'0 else false
     | _ -> false)
  | Ckarrow (ck, ck') ->
    (match x0 with
     | Ckarrow (ck0, ck'0) -> if eq_clock ck ck0 then eq_clock ck' ck'0 else false
     | _ -> false)

type sign =
| Signcons of pattern * clock * pattern * clock

type s = identifier list

type h = (char list*sign) list

(** val assoc : char list -> c -> clock option **)

let rec assoc x = function
| [] -> None
| p :: l0 -> let y,c0 = p in if eq_identifier x y then Some c0 else assoc x l0

(** val assoc_global : char list -> h -> sign option **)

let rec assoc_global x = function
| [] -> None
| p :: l0 -> let y,c0 = p in if eq_identifier x y then Some c0 else assoc_global x l0

(** val apply_subst : clock -> identifier -> identifier -> clock **)

let rec apply_subst c0 x' y' =
  match c0 with
  | Ckbase -> c0
  | Ckon (c1, x) ->
    if eq_identifier x x' then Ckon ((apply_subst c1 x' y'), y') else Ckon ((apply_subst c1 x' y'), x)
  | Ckonnot (c1, x) ->
    if eq_identifier x x'
    then Ckonnot ((apply_subst c1 x' y'), y')
    else Ckonnot ((apply_subst c1 x' y'), x)
  | Cktuple (ck1, ck2) -> Cktuple ((apply_subst ck1 x' y'), (apply_subst ck2 x' y'))
  | Ckarrow (ck1, ck2) -> Ckarrow ((apply_subst ck1 x' y'), (apply_subst ck2 x' y'))

(** val apply_substs : (identifier*identifier) list -> clock -> clock **)

let rec apply_substs s0 c0 =
  match s0 with
  | [] -> c0
  | y0 :: sigma' -> let x,y = y0 in apply_substs sigma' (apply_subst c0 x y)

(** val carriers : clock -> s **)

let rec carriers = function
| Ckbase -> []
| Ckon (ck, x) -> x :: (carriers ck)
| Ckonnot (ck, x) -> x :: (carriers ck)
| Cktuple (ck1, ck2) -> app (carriers ck1) (carriers ck2)
| Ckarrow (ck1, ck2) -> app (carriers ck1) (carriers ck2)

(** val subst_ck : clock -> clock -> clock **)

let rec subst_ck x1 x2 =
  match x1 with
  | Ckbase -> x2
  | Ckon (ck, x) -> Ckon ((subst_ck ck x2), x)
  | Ckonnot (ck, x) -> Ckonnot ((subst_ck ck x2), x)
  | Cktuple (ck1, ck2) -> Cktuple ((subst_ck ck1 x2), (subst_ck ck2 x2))
  | Ckarrow (ck1, ck2) -> Ckarrow ((subst_ck ck1 x2), (subst_ck ck2 x2))

(** val mem_S : char list -> identifier list -> bool **)

let rec mem_S x = function
| [] -> false
| y :: l0 -> if eq_identifier x y then true else mem_S x l0

(** val e_subst_fun_var :
    identifier -> identifier -> identifier list -> (identifier*identifier) list option **)

let e_subst_fun_var x y s0 =
  if mem_S x s0 then Some ((x,y) :: []) else Some []

(** val e_subst_fun_exp :
    identifier -> lexps -> identifier list -> (identifier*identifier) list option **)

let e_subst_fun_exp x e s0 =
  match e with
  | Esone_exp e0 ->
    (match e0 with
     | Evar y -> e_subst_fun_var x y s0
     | _ -> if negb (mem_S x s0) then Some [] else None)
  | Escons_exps (_, _) -> None

(** val e_subst_fun : pattern -> lexps -> identifier list -> (identifier*identifier) list option **)

let rec e_subst_fun p e s0 =
  match p with
  | Patp_unit ->
    (match e with
     | Esone_exp e0 -> (match e0 with
                        | Eunit _ -> Some []
                        | _ -> None)
     | Escons_exps (_, _) -> None)
  | Patp_var x ->
    (match e with
     | Esone_exp e0 ->
       (match e0 with
        | Evar y -> e_subst_fun_var x y s0
        | _ -> if negb (mem_S x s0) then Some [] else None)
     | Escons_exps (_, _) -> None)
  | Patp_tuple (x, p') ->
    (match e with
     | Esone_exp _ -> None
     | Escons_exps (e0, es') ->
       let s1 = e_subst_fun_exp x (Esone_exp e0) s0 in
       let s2 = e_subst_fun p' es' s0 in
       (match s1 with
        | Some l -> (match s2 with
                     | Some l' -> Some (app l l')
                     | None -> None)
        | None -> None))

(** val p_subst_fun_vars :
    identifier -> identifier -> identifier list -> (identifier*identifier) list option **)

let p_subst_fun_vars x y s0 =
  if mem_S x s0 then Some ((x,y) :: []) else Some []

(** val p_subst_fun : pattern -> pattern -> identifier list -> (identifier*identifier) list option **)

let rec p_subst_fun p p' s0 =
  match p with
  | Patp_unit -> Some []
  | Patp_var x ->
    (match p' with
     | Patp_var y -> if mem_S x s0 then Some ((x,y) :: []) else Some []
     | _ -> if negb (mem_S x s0) then Some [] else None)
  | Patp_tuple (x, p1') ->
    (match p' with
     | Patp_tuple (y, p2') ->
       let s1 = p_subst_fun_vars x y s0 in
       let s2 = p_subst_fun p1' p2' s0 in
       (match s1 with
        | Some l -> (match s2 with
                     | Some l' -> Some (app l l')
                     | None -> None)
        | None -> None)
     | _ -> None)

(** val clockof_exp : c -> lexp -> clock option **)

let rec clockof_exp c0 = function
| Eunit ck -> Some ck
| Econst (_, ck) -> Some ck
| Evar x -> assoc x c0
| Econstructor (_, ck) -> Some ck
| Ebinop (e1, _, e2) ->
  let c1 = clockof_exp c0 e1 in
  let c2 = clockof_exp c0 e2 in
  (match c1 with
   | Some ck1 ->
     (match c2 with
      | Some ck2 -> if eq_clock ck1 ck2 then Some ck1 else None
      | None -> None)
   | None -> None)
| Eunop (_, e) -> clockof_exp c0 e
| Ewhen (e, x) ->
  let c1 = clockof_exp c0 e in
  let c2 = assoc x c0 in
  (match c1 with
   | Some ck1 ->
     (match c2 with
      | Some ck2 -> if eq_clock ck1 ck2 then Some (Ckon (ck1, x)) else None
      | None -> None)
   | None -> None)
| Ewhennot (e, x) ->
  let c1 = clockof_exp c0 e in
  let c2 = assoc x c0 in
  (match c1 with
   | Some ck1 ->
     (match c2 with
      | Some ck2 -> if eq_clock ck1 ck2 then Some (Ckonnot (ck1, x)) else None
      | None -> None)
   | None -> None)

(** val clockof_exps : c -> lexps -> clock option **)

let rec clockof_exps c0 = function
| Esone_exp e -> clockof_exp c0 e
| Escons_exps (e, es0) ->
  (match clockof_exp c0 e with
   | Some ck -> (match clockof_exps c0 es0 with
                 | Some cks -> Some (Cktuple (ck, cks))
                 | None -> None)
   | None -> None)

(** val eq_ident : identifier -> identifier -> bool **)

let eq_ident x y =
  if eq_identifier x y then true else false

(** val eq_ck : clock -> clock -> bool **)

let eq_ck x y =
  if eq_clock x y then true else false

(** val clockof_cexp : c -> cexp -> clock option **)

let rec clockof_cexp c0 = function
| Ceexp e -> clockof_exp c0 e
| Cemerge (x, e1, e2) ->
  let cx = assoc x c0 in
  let c1 = clockof_cexp c0 e1 in
  let c2 = clockof_cexp c0 e2 in
  (match cx with
   | Some ck ->
     (match c1 with
      | Some c3 ->
        (match c3 with
         | Ckon (ck', x') ->
           (match c2 with
            | Some c4 ->
              (match c4 with
               | Ckonnot (ck'', x'') ->
                 if (&&) ((&&) ((&&) (eq_ident x x') (eq_ident x' x'')) (eq_ck ck ck'))
                      (eq_ck ck' ck'')
                 then Some ck
                 else None
               | _ -> None)
            | None -> None)
         | _ -> None)
      | None -> None)
   | None -> None)
| Ceif (e, t, f) ->
  let ce = clockof_exp c0 e in
  let ct = clockof_cexp c0 t in
  let cf = clockof_cexp c0 f in
  (match ce with
   | Some a ->
     (match ct with
      | Some b ->
        (match cf with
         | Some c1 -> if (&&) (eq_ck a b) (eq_ck b c1) then Some c1 else None
         | None -> None)
      | None -> None)
   | None -> None)

(** val clockof_pat : c -> pattern -> clock option **)

let rec clockof_pat c0 = function
| Patp_unit -> Some Ckbase
| Patp_var x -> assoc x c0
| Patp_tuple (x, p2) ->
  let c1 = assoc x c0 in
  let c2 = clockof_pat c0 p2 in
  (match c1 with
   | Some ck1 -> (match c2 with
                  | Some ck2 -> Some (Cktuple (ck1, ck2))
                  | None -> None)
   | None -> None)

(** val well_clocked_equation : equation -> h -> c -> bool **)

let well_clocked_equation eqn h0 c0 =
  match eqn with
  | EqDef (p, ck, ce) ->
    let c1 = clockof_pat c0 p in
    let c2 = clockof_cexp c0 ce in
    (match c1 with
     | Some ck1 -> (match c2 with
                    | Some ck2 -> (&&) (eq_ck ck1 ck2) (eq_ck ck2 ck)
                    | None -> false)
     | None -> false)
  | EqFby (p, ck, _, e) ->
    let c1 = clockof_pat c0 p in
    let c2 = clockof_exp c0 e in
    (match c1 with
     | Some ck1 -> (match c2 with
                    | Some ck2 -> (&&) (eq_ck ck1 ck2) (eq_ck ck2 ck)
                    | None -> false)
     | None -> false)
  | EqApp (p, ck, f, es) ->
    let sigf = assoc_global f h0 in
    (match sigf with
     | Some s0 ->
       let Signcons (p1, ck1, p2, ck2) = s0 in
       let ckp = clockof_pat c0 p in
       let ckes = clockof_exps c0 es in
       (match ckp with
        | Some ckp0 ->
          (match ckes with
           | Some ckes0 ->
             let s1 = app (carriers ck1) (carriers ck2) in
             let sigma = e_subst_fun p1 es s1 in
             let sigma' = p_subst_fun p2 p s1 in
             (match sigma with
              | Some sigma0 ->
                (match sigma' with
                 | Some sigma'0 ->
                   let cke_subst = apply_substs sigma0 (subst_ck ck1 ck) in
                   let ckp_subst = apply_substs (app sigma'0 sigma0) (subst_ck ck2 ck) in
                   (&&) (eq_ck ckes0 cke_subst) (eq_ck ckp0 ckp_subst)
                 | None -> false)
              | None -> false)
           | None -> false)
        | None -> false)
     | None -> false)
  | EqEval (p, ck, _) ->
    let c1 = clockof_pat c0 p in
    (match c1 with
     | Some c2 -> (match c2 with
                   | Ckbase -> (match ck with
                                | Ckbase -> true
                                | _ -> false)
                   | _ -> false)
     | None -> false)

(** val well_clocked_eqns : leqns -> h -> c -> bool **)

let rec well_clocked_eqns eqns h0 c0 =
  match eqns with
  | Eqseqs_one eq -> well_clocked_equation eq h0 c0
  | Eqseqs_cons (eq, eqs) -> (&&) (well_clocked_equation eq h0 c0) (well_clocked_eqns eqs h0 c0)

(** val clockof_node : nodedef -> h -> c -> sign option **)

let clockof_node node h0 c0 =
  let Nodemk_node (_, p, p', eqns) = node in
  let c1 = clockof_pat c0 p in
  let c' = clockof_pat c0 p' in
  (match c1 with
   | Some ckp ->
     (match c' with
      | Some ckp' -> if well_clocked_eqns eqns h0 c0 then Some (Signcons (p, ckp, p', ckp')) else None
      | None -> None)
   | None -> None)

(** val well_clocked_prog : (c*nodedef) list -> h -> bool **)

let rec well_clocked_prog prog h0 =
  match prog with
  | [] -> true
  | y :: nodes ->
    let c0,y0 = y in
    let Nodemk_node (f, p, p', eqns) = y0 in
    let sign0 = clockof_node (Nodemk_node (f, p, p', eqns)) h0 c0 in
    (match sign0 with
     | Some s0 -> well_clocked_prog nodes ((f,s0) :: h0)
     | None -> false)
