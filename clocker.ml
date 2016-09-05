open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl

type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | CtUnknown
  | CVar of vartype
  | CTuple of ct list
  | Arrow of ct * ct
  | On of ct * ident
  | Carrier of ident * ct
and vartype = { c_index : int ; mutable c_value : ct }
and scheme = Forall of int list * ct (* arrow ? *)

exception TypingBug of string

type env = (string * ct) list

let cpt = ref 0

let new_vartype, reset_vartypes =
  (function () -> incr cpt;
     { c_index = !cpt; c_value = CtUnknown }),
  (function () -> cpt := 0)


let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be an ident"


let typing_env = ref []

let typing_scheme_env = ref []


let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else ((name_of q)^s)
  in "'"^(name_of n)

let rec print_tuple f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a * " f h ; print_tuple f fmt t
  | _ -> ()


let rec print_type fmt = function

  | CVar {c_index = n ; c_value = CtUnknown } ->
    (*let name = try List.assoc n tvar_names
      with Not_found ->
        raise (TypingBug ("Non generic variable :"^(string_of_int n)))
      in *) Format.fprintf Format.std_formatter "%s" (tvar_name n)
  | CVar {c_index = _ ; c_value = t} -> print_type fmt t
  | Arrow (t1,t2) -> Format.fprintf fmt "(%a -> %a)" print_type t1 print_type t2
  | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_type) tl
  | On (x,i) ->  Format.fprintf fmt "%a on %s" print_type x i
  | CtUnknown -> Format.fprintf fmt  "?"
  | Carrier (i,x) -> Format.fprintf fmt "(%s : %a)" i print_type x

let rec add_pat_to_env i env =
  match i.p_desc with
  | Ident id ->
    let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | PUnit -> let var = incr cpt; CVar { c_index = !cpt; c_value = CtUnknown } in
    env := (i.p_desc,var) :: !env ;
  | Tuple tl -> List.iter (fun i -> add_pat_to_env i env) tl

let rec add_to_env i tau env =
match i.p_desc with
| Ident id ->
  env := (i.p_desc,tau) :: !env ;
| PUnit ->
  env := (i.p_desc,tau) :: !env ;
| Tuple il ->
  match tau with
  | CTuple tl -> let lit = List.combine il tl in
    List.iter (fun (i,t) -> (add_to_env i t env)) lit
  | _ -> failwith "not a tuple ..."


let print_env fmt gamma =
  List.iter (fun (x,t) -> Format.fprintf Format.std_formatter "%a :: %a \n"
                print_pattern {p_desc = x ; p_loc = Location.none}
                print_type t)
    gamma

let rec get_type p env =
  match p.p_desc with
  | Ident i -> List.assoc p.p_desc env
  | PUnit -> List.assoc p.p_desc env
  | Tuple pl ->
    let tl = List.map (fun p -> List.assoc p.p_desc env) pl in
    CTuple tl


let rec shorten_var t =
  match t with
  | CVar { c_index = _ ; c_value= CtUnknown} -> t
  | CVar ({ c_index = _ ; c_value = CVar {c_index = m ; c_value = CtUnknown}})->
    CVar {c_index = m ; c_value = CtUnknown}
  | CVar ({c_index = _ ; c_value = CVar tv1} as tv2) ->
    tv2.c_value <- tv1.c_value;
    shorten_var t
  | CVar {c_index = _ ; c_value = t'} -> t'
  (* )  | CtUnknown -> raise (TypingBug "shorten" ) *)
  | t' -> t'

exception TypeClash of ct * ct

let occurs {c_index = n; c_value = _ } =
  let rec occrec = function
    | CVar {c_index = m; c_value = _ } -> (n = m)
    | Arrow(ct1, ct2) -> occrec ct1 || occrec ct2
    | CTuple (ctl) -> List.fold_left (fun acc ctl -> occrec ctl || acc) false ctl
    | On (x,i) -> occrec x
    | Carrier (i,x) -> occrec x
    | _  -> raise (TypingBug "occurs")
  in occrec

let rec unify (tau1, tau2) =
  match (shorten_var tau1, shorten_var tau2) with
  | (CVar ({c_index = n ;c_value =CtUnknown}) as t1),
    (CVar ({c_index = m ;c_value =CtUnknown} as tv2))
    ->
    if n <> m then tv2.c_value <- t1
  | t1, (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.c_value <- t1
    else raise (TypeClash (t1,t2))
  | (CVar ({c_index = _ ;c_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.c_value <- t2
    else raise (TypeClash (t1,t2))
  | Arrow (t1,t2) , Arrow (t'1, t'2)
    -> unify (t1,t'1) ; unify (t2,t'2)
  | CTuple ctl1 , CTuple ctl2 ->
    let ll = List.combine ctl1 ctl2 in
    List.iter unify ll
  | On (ct1,i1) , On (ct2, i2) when i1 = i2 ->
    unify (ct1, ct2)

  | (t1,t2) -> raise (TypeClash (t1,t2))


let vars_of_type tau =
  let rec vars vs ck =
    match ck with
    | CVar {c_index = n ; c_value = CtUnknown} ->
      if List.mem n vs then vs else n::vs
    | CVar {c_index = _ ; c_value = t} -> vars vs t
    | Arrow (t1,t2) -> vars (vars vs t1) t2
    | CTuple (c::ctl) ->
      List.fold_left (fun acc t -> vars acc t) (vars vs c) ctl
    | On (x,i) -> vars vs x
    | Carrier (i,x) -> vars vs x
    | CtUnknown | _ -> raise (TypingBug "vars_of_type")

  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_type bv t = substract (vars_of_type t) bv

let unknowns_of_type_env env = List.flatten
    (List.map (fun (id,t) -> (unknowns_of_type [] t)) env)

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalise_type (gamma,tau) =
  let fv = vars_of_type tau in
  let genvars = make_set fv  in
  Forall (genvars, tau)

let inst (Forall(gv,ct)) =
  let unknowns = List.map (function n -> n, CVar (new_vartype ())) gv in
  let rec ginstance ct =
    match ct with
    | CVar { c_index = n ; c_value = CtUnknown } ->
      begin
        try List.assoc n unknowns
        with Not_found -> ct
      end
    | CVar {c_index = _ ; c_value = t } -> ginstance t
    | CTuple tl -> CTuple (List.map ginstance tl)
    | Arrow (t1,t2) -> Arrow (ginstance t1, ginstance t2)
    | On (x,i) -> On(ginstance x,i)
    | Carrier (i,x) -> Carrier (i, ginstance x)
    | CtUnknown -> raise (TypingBug "inst")

  in ginstance ct


let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple pl -> List.fold_left (fun acc p -> (string_of_pattern p)@acc) [] pl
  | PUnit -> []


let print_type_scheme fmt (Forall(gv,t)) =
let names = let rec names_of = function
               | (n,[]) -> []
               | (n,(v1::lv)) -> (tvar_name n)::(names_of (n+1,lv))
             in names_of (1,gv) in
let tvar_names = List.combine (List.rev gv) names in
let rec print_string_list fmt l =
  match l with
  | [] -> ()
  | [h] -> Format.fprintf fmt "%s" h
  | h :: t -> Format.fprintf fmt "%s,%a" h print_string_list t  in
  let rec print_rec fmt = function
    | CVar {c_index = n ; c_value = CtUnknown } ->
      let name = try List.assoc n tvar_names
        with Not_found ->
          raise (TypingBug ("Non generic variable :"^(string_of_int n)))
      in Format.fprintf Format.std_formatter "%s" name
    | CVar {c_index = _ ; c_value = t} -> print_rec fmt t
    | Arrow (t1,t2) -> Format.fprintf fmt "(%a -> %a)" print_rec t1 print_rec t2
    | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_rec) tl
    | On (x,i) ->  Format.fprintf fmt "%a on %s" print_rec x i;
    | CtUnknown -> Format.fprintf fmt  "?"
    | Carrier (i,x) -> Format.fprintf fmt "(%s : %a)" i print_rec x;
in  Format.fprintf Format.std_formatter "forall %a . %a" print_string_list names
  print_rec t


  let rec typing_expr gamma =
    let rec type_rec { e_desc = e ; e_loc = _} =
      match e with
      | Value _ -> CVar (new_vartype () )
      | Variable n ->begin
          try
            let sigma =
              get_type { p_desc = (Ident n) ; p_loc = Location.none } gamma in
          sigma
          with  Not_found -> CVar (new_vartype ())
        end
      | Alternative (e1,e2,e3) ->
        let t1 = type_rec e1 in
        let t2 = type_rec e2 in
        let t3 = type_rec e3 in
        unify (t1,t2);
        unify (t2,t3); t3
      | Application (i,e2) ->
        let t1 = List.assoc i !typing_scheme_env in
        let u = CVar (new_vartype ()) in
        let t1' = inst t1 in
        unify (t1' , Arrow (type_rec e2, u)); u
      | ETuple t -> CTuple (List.map type_rec t)
      | InfixOp (op, e1,e2) ->
        let t1 = type_rec e1 in
        let t2 = type_rec e2 in
        unify (t1,t2) ; t1
      | PrefixOp (op, e1) -> type_rec e1
      | Unit ->
        CVar (new_vartype ())
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
        let i =  get_ident e2 in
        unify (t1,t2) ;
        begin match t2 with
        | CVar ev -> (ev.c_value <- Carrier (i,ev.c_value))
        | _ -> ()
        end ;
        On (t1,i)
      | Pre e -> type_rec e

    in
    type_rec

let typing_equation { pattern = p ; expression = e} =
  let tau =
    try typing_expr !typing_env e
    with TypeClash(t1,t2) ->
      let vars = (vars_of_type t1)@(vars_of_type t2) in
      print_string " Clock clash between ";
      print_type_scheme Format.std_formatter (Forall(vars,t1));
      print_string " and ";
      print_type_scheme Format.std_formatter (Forall(vars,t2));
      print_newline ();
      raise (Failure "clocking") in
  add_to_env p tau typing_env

let type_node node tse =
  typing_env := [];
  typing_scheme_env := !tse;
  reset_vartypes ();
  add_pat_to_env node.inputs typing_env;
  List.iter (fun e -> typing_equation e) node.equations;


  let lin =
    try get_type node.inputs !typing_env
    with Not_found ->
      print_pattern Format.std_formatter node.inputs;
      failwith "lin"
  in
  let lout =
    try get_type node.outputs !typing_env
    with Not_found -> print_pattern Format.std_formatter node.outputs;
      failwith "lout"
  in
  let tt = Arrow (lin, lout) in
  let ts = (generalise_type (!typing_env,tt)) in
  (List.hd (string_of_pattern node.name), ts)::!typing_scheme_env
