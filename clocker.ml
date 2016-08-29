open Parsing_ast

type clock =
  | Clock_exp of ct
  | Clock_flow of ck
  | Clock_scheme of scheme
and
  ct =
  | Base
  | CtUnknown
  | ExpVar of exptype
  | Tuple of ct list
  | Flowck of ck
  | Arrow of ct * ct
and ck =
  | CkUnknown
  | FlowVar of flowtype
  | On of ck * ident
  | OnNot of ck * ident
and exptype = { e_index : int ; mutable e_value : ct }
and flowtype = { f_e_index : int ; mutable f_value : ck }
and scheme = Forall of int list * ct (* arrow *)

exception TypingBug of string

let init_env = []
let init_typing_env = List.map
    (function s -> (s,Forall([],
                             Arrow
                               (ExpVar { e_index = 1 ; e_value = CtUnknown },
                                ExpVar { e_index = 1 ; e_value = CtUnknown }
                             )
                            )
                   )
    )
    init_env

let new_exptype, reset_exptypes =
  let counter = ref 0 in
  (function () -> counter := !counter + 1;
    { e_index = !counter; e_value = CtUnknown }),
  (function () -> counter := 0)

let rec shorten_exp t =
  match t with
  | ExpVar { e_index = _ ; e_value= CtUnknown} -> t
  | ExpVar ({ e_index = _ ; e_value = ExpVar {e_index = _ ; e_value = CtUnknown} as tv}) -> tv
  | ExpVar ({e_index = _ ; e_value = ExpVar tv1} as tv2) -> tv2.e_value <- tv1.e_value; shorten_exp t
  | ExpVar {e_index = _ ; e_value = t'} -> t'
  | CtUnknown -> raise (TypingBug "shorten" )
  | t' -> t'

exception TypeClash of ct * ct

let occurs {e_index = n; e_value = _ } =
  let rec occrec = function
    | ExpVar {e_index = m; e_value = _ } -> (n = m)
    | Tuple (ct1, ct2) -> occrec ct1 || occrec ct2
    | _  -> raise (TypingBug "occurs")
  in occrec

let rec unify (tau1, tau2) =
  match (shorten_exp tau1, shorten_exp tau2) with
  | (ExpVar ({e_index = n ;e_value =CtUnknown} as tv1)),
    (ExpVar ({e_index = m ;e_value =CtUnknown}) as t2)
    -> if n <> m then tv1.e_value <- t2
  | t1, (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.e_value <- t1
    else raise (TypeClash (t1,t2))
  | (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.e_value <- t2
    else raise (TypeClash (t1,t2))
  | Tuple (t1,t2) , Tuple (t'1, t'2)
    -> unify (t1,t'1) ; unify (t2,t'2)
  | (t1,t2) -> raise (TypeClash (t1,t2))


  let vars_of_type tau =
    let rec vars vs = function

      | ExpVar {e_index = n ; e_value = CtUnknown} -> if List.mem n vs then vs else n::vs
      | ExpVar {e_index = _ ; e_value = t} -> vars vs t
      | CtUnknown | _ -> raise (TypingBug "vars_of_type")

    in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1 (* ? *)

let unknowns_of_type (bv,t) = substract (vars_of_type t) bv

let flat = List.flatten

let unknowns_of_type_env env = flat (List.map (function (id,Forall(gv,t)) -> unknowns_of_type (gv,t)) env)

let rec make_set = function
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalise_type (gamma,tau) =
  let genvars = make_set (substract (vars_of_type tau) (unknowns_of_type_env gamma)) in
  Forall (genvars, tau)

  let gen_instance (Forall(gv,tau)) =
    let unknowns = List.map (function n -> n, ExpVar (new_exptype () )) gv in
    let rec ginstance = function
      | ( ExpVar { e_index = n ; e_value = CtUnknown } as t) ->
        (try List.assoc n unknowns with Not_found -> t)
      | ExpVar {e_index = _ ; e_value = t } -> ginstance t
      | Tuple tl -> Tuple (List.map ginstance tl)
      | Arrow (t1,t2) -> Arrow (ginstance t1, ginstance t2)
      | CtUnknown | _ -> raise (TypingBug "gen_instance")

    in ginstance tau

    let rec typing_expr gamma =
      let rec type_rec { e_desc = e ; e_loc = _} =
        match e with
        | Value _ -> Base
        | Variable n ->begin try
              let sigma =  List.assoc n gamma in
            gen_instance sigma
            with
            Not_found -> raise (TypingBug "Unbound")
          end
        | Alternative (e1,e2,e3) ->
          let ev = ExpVar (new_exptype ()) in
          unify (ev, type_rec e1);
          let t2 = type_rec e2 and t3 = type_rec e3 in
          unify (t2,t3); t3
        | Application (i,e2) -> failwith "todo"
        | ETuple t -> Tuple (List.map type_rec t)
        | _ -> failwith "todo"
(*
          let u = ExpVar (new_exptype ())
          in unify (_, Arrow(type_rec e2,u)); u *)

      in
      type_rec 
