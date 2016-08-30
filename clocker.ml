open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl

type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | Base
  | CtUnknown
  | ExpVar of exptype
  | Tuple of ct list
  | Arrow of ct * ct
  | On of ct * ident
and exptype = { e_index : int ; mutable e_value : ct }
and scheme = Forall of int list * ct (* arrow *)

exception TypingBug of string

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be an ident"


let init_env = ["+";"-";"*";"/";"="]
let global_env = ref init_env
let init_typing_env = List.map
    (function s -> (s,Forall([1],
                             Arrow(ExpVar { e_index = 1 ; e_value = CtUnknown },
                             Arrow
                               (ExpVar { e_index = 1 ; e_value = CtUnknown },
                                ExpVar { e_index = 1 ; e_value = CtUnknown }
                               )
                                  )
                            )
                   )
    )
    init_env


let global_typing_env = ref init_typing_env


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
  (* )  | CtUnknown -> raise (TypingBug "shorten" ) *)
  | t' -> t'

exception TypeClash of ct * ct

let occurs {e_index = n; e_value = _ } =
  let rec occrec = function
    | ExpVar {e_index = m; e_value = _ } -> (n = m)
    | Arrow(ct1, ct2) -> occrec ct1 || occrec ct2
    | Tuple (ctl) -> List.fold_left (fun acc ctl -> occrec ctl || acc) false ctl
    | On (x,i) -> occrec x
    | Base -> false
    | _  -> raise (TypingBug "occurs")
  in occrec

let rec unify (tau1, tau2) =
  match (shorten_exp tau1, shorten_exp tau2) with
  | Base, Base -> ()
  | (ExpVar ({e_index = n ;e_value =CtUnknown} as tv1)),
    (ExpVar ({e_index = m ;e_value =CtUnknown}) as t2)
    -> if n <> m then tv1.e_value <- t2
  | t1, (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.e_value <- t1
    else raise (TypeClash (t1,t2))
  | (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.e_value <- t2
    else raise (TypeClash (t1,t2))
  | Arrow (t1,t2) , Arrow (t'1, t'2)
    -> unify (t1,t'1) ; unify (t2,t'2)
  | Tuple ctl1 , Tuple ctl2 ->
    let ll = List.combine ctl1 ctl2 in
    List.iter unify ll
  | On (ct1,i1) , On (ct2, i2) when i1 = i2 ->
      unify (ct1, ct2)

  | (t1,t2) -> raise (TypeClash (t1,t2))


  let vars_of_type tau =
    let rec vars vs = function

      | ExpVar {e_index = n ; e_value = CtUnknown} -> if List.mem n vs then vs else n::vs
      | ExpVar {e_index = _ ; e_value = t} -> vars vs t
      | Arrow (t1,t2) -> vars (vars vs t1) t2
      | Tuple ctl -> failwith "todo tuple"
      | Base -> vs
      | On (x,i) -> vars vs x
      | CtUnknown -> raise (TypingBug "vars_of_type")

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
    let unknowns = List.map (function n -> n, ExpVar (new_exptype ())) gv in
    let rec ginstance = function
      | ( ExpVar { e_index = n ; e_value = CtUnknown } as t) ->
        (try List.assoc n unknowns with Not_found -> t)
      | ExpVar {e_index = _ ; e_value = t } -> ginstance t
      | Tuple tl -> Tuple (List.map ginstance tl)
      | Arrow (t1,t2) -> Arrow (ginstance t1, ginstance t2)
      | Base -> Base
      | On (x,i) -> On(ginstance x,i)
      | CtUnknown -> raise (TypingBug "gen_instance")

    in ginstance tau

    let rec typing_expr gamma =
      let rec type_rec { e_desc = e ; e_loc = _} =
        match e with
        | Value _ -> ExpVar (new_exptype () )
        | Variable n ->begin try
              let sigma =  List.assoc n gamma in
            gen_instance sigma
            with
              Not_found -> CtUnknown (*) raise (TypingBug "Unbound") *)
          end
        | Alternative (e1,e2,e3) ->
          (*let ev = ExpVar (new_exptype ()) in
            unify (ev, type_rec e1); *)
          let t1 = type_rec e1 in
          let t2 = type_rec e2 in
          let t3 = type_rec e3 in
          unify (t1,t2);
          unify (t2,t3); t3
        | Application (i,e2) ->
          let t2 = type_rec e2 in t2
        | ETuple t -> Tuple (List.map type_rec t)
        | InfixOp (op, e1,e2) ->
          let t1 = type_rec e1 in
          let t2 = type_rec e2 in
          unify (t1,t2) ; t1
        | PrefixOp (op, e1) -> type_rec e1
        | Unit -> Base
        | Fby (e1,e2) ->
          let t1 = type_rec e1 in
          let t2 = type_rec e2 in
          unify (t1,t2); t1
          | Arrow (e1,e2) ->
            let t1 = type_rec e1 in
            let t2 = type_rec e2 in
            unify (t1,t2) ; t1
          | When (e1,e2) ->
          let t1 = type_rec e1 in
          let t2 = type_rec e2 in
          unify (t1,t2) ;
          let i =  get_ident e2 in
          On (t1,i)
        | Pre e -> type_rec e

      in
      type_rec


let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else ((name_of q)^s)
  in "'"^(name_of n)

let print_type_scheme (Forall(gv,t)) =
  let names = let rec names_of = function
      | (n,[]) -> []
      | (n,(v1::lv)) -> (tvar_name n)::(names_of (n+1,lv))
    in names_of (1,gv) in
  let tvar_names = List.combine (List.rev gv) names in
  let rec print_rec = function
    | ExpVar {e_index = n ; e_value = CtUnknown } ->
      let name = try List.assoc n tvar_names
        with Not_found ->
          raise (TypingBug "Non generic variable")
      in print_string name
    | ExpVar {e_index = _ ; e_value = t} -> print_rec t
    | Arrow (t1,t2) -> print_string "("; print_rec t1;
      print_string " -> "; print_rec t2;
      print_string ")"
    | Tuple tl -> List.iter print_rec tl
    | Base -> print_string "Base"
    | On (x,i) ->  print_rec x; print_string " on "; print_string i;
    | CtUnknown -> raise (TypingBug "print_type_scheme")


  in print_rec t



let print_env gamma =
  List.iter (fun (x,scheme) -> Printf.printf "%s :: " x; (print_type_scheme scheme); print_endline "") gamma


let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> i
  | Tuple pl -> List.fold_left (fun acc p -> string_of_pattern p^acc) "" pl

let typing eq =

  Format.fprintf Format.std_formatter "I try to clock the equation : %a \n" print_equation eq;

  reset_exptypes ();
  let tau =
    try typing_expr !global_typing_env eq.expression
    with TypeClash(t1,t2) ->
      let vars = (vars_of_type t1)@(vars_of_type t2) in
      print_string " Clock clash between ";
      print_type_scheme (Forall(vars,t1));
      print_string " and ";
      print_type_scheme (Forall(vars,t2));
      print_newline ();
      raise (Failure "clocking") in
  let sigma = generalise_type (!global_typing_env,tau) in
  let s = string_of_pattern eq.pattern in
  global_env := s::!global_env ;
  global_typing_env := (s,sigma) :: !global_typing_env ;
  reset_exptypes ();

print_env !global_typing_env

let tuple_of_list l = Tuple l

let type_node n =
  let ll = List.map (fun i -> (string_of_pattern i), Forall([1],ExpVar { e_index = 1 ; e_value = CtUnknown })) n.inputs in
  global_typing_env := ll@(!global_typing_env);

  List.iter typing n.equations;
  print_env !global_typing_env;

  let inputs_strings = List.map string_of_pattern n.inputs in
  let outputs_strings = List.map string_of_pattern n.outputs in
  let input_clocks = List.map (fun i -> List.assoc i !global_typing_env) inputs_strings in
  let output_clocks = List.map (fun o -> List.assoc o !global_typing_env) outputs_strings in
  Printf.printf "node clock is : ";
  List.iter print_type_scheme input_clocks;
  Printf.printf " -> ";
  List.iter print_type_scheme output_clocks;
  print_endline ""
