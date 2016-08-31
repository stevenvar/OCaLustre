open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl

type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | CtUnknown
  | ExpVar of exptype
  | CTuple of ct list
  | Arrow of ct * ct
  | On of ct * ident
and exptype = { e_index : int ; mutable e_value : ct }
and scheme = Forall of int list * ct (* arrow ? *)

exception TypingBug of string

type env = (string * ct) list

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be an ident"


let typing_env = ref []

let rec get_type v env =
  let rec aux p v =
    match p.p_desc with
    | Ident i -> i = v
    | Tuple tl -> List.fold_left (fun acc t -> aux t v || acc) false tl
  in
  match env with
  |  [] -> failwith "not found "
  | (i,ck)::t -> if aux i v then ck else get_type v t

let new_exptype, reset_exptypes =
  let counter = ref 0 in
  (function () -> incr counter;
     { e_index = !counter; e_value = CtUnknown }),
  (function () -> counter := 0)

let rec shorten_exp t =
  match t with
  | ExpVar { e_index = _ ; e_value= CtUnknown} -> t
  | ExpVar ({ e_index = _ ; e_value = ExpVar {e_index = m ; e_value = CtUnknown}}) -> ExpVar {e_index = m ; e_value = CtUnknown}
  | ExpVar ({e_index = _ ; e_value = ExpVar tv1} as tv2) -> tv2.e_value <- tv1.e_value; shorten_exp t
  | ExpVar {e_index = _ ; e_value = t'} -> t'
  (* )  | CtUnknown -> raise (TypingBug "shorten" ) *)
  | t' -> t'

exception TypeClash of ct * ct

let occurs {e_index = n; e_value = _ } =
  let rec occrec = function
    | ExpVar {e_index = m; e_value = _ } -> (n = m)
    | Arrow(ct1, ct2) -> occrec ct1 || occrec ct2
    | CTuple (ctl) -> List.fold_left (fun acc ctl -> occrec ctl || acc) false ctl
    | On (x,i) -> occrec x
    | _  -> raise (TypingBug "occurs")
  in occrec

let rec unify (tau1, tau2) =
  match (shorten_exp tau1, shorten_exp tau2) with
  | (ExpVar ({e_index = n ;e_value =CtUnknown}) as t1),
    (ExpVar ({e_index = m ;e_value =CtUnknown} as tv2))
    ->
    if n <> m then tv2.e_value <- t1
  | t1, (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t2)
    -> if not (occurs tv t1) then tv.e_value <- t1
    else raise (TypeClash (t1,t2))
  | (ExpVar ({e_index = _ ;e_value =CtUnknown} as tv) as t1) , t2
    -> if not (occurs tv t2) then tv.e_value <- t2
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
    | ExpVar {e_index = n ; e_value = CtUnknown} -> if List.mem n vs then vs else n::vs
    | ExpVar {e_index = _ ; e_value = t} -> vars vs t
    | Arrow (t1,t2) -> vars (vars vs t1) t2
    | CTuple (c::ctl) -> List.fold_left (fun acc t -> vars acc t) (vars vs c) ctl
    | On (x,i) -> vars vs x
    | CtUnknown | _ -> raise (TypingBug "vars_of_type")

  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_type bv t = substract (vars_of_type t) bv

let unknowns_of_type_env env = List.flatten
    (List.map (fun (id,Forall(gv,t)) -> (unknowns_of_type gv t)) env)

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalise_type (gamma,tau) =
  let fv = (substract (vars_of_type tau) (unknowns_of_type_env gamma)) in
  let genvars = make_set fv  in
  Forall (genvars, tau)

let inst (Forall(gv,ct)) =
  let unknowns = List.map (function n -> n, ExpVar (new_exptype ())) gv in
  let rec ginstance ct =
    match ct with
    | ExpVar { e_index = n ; e_value = CtUnknown } ->
      begin
        try List.assoc n unknowns
        with Not_found -> ct
      end
    | ExpVar {e_index = _ ; e_value = t } -> ginstance t
    | CTuple tl -> CTuple (List.map ginstance tl)
    | Arrow (t1,t2) -> Arrow (ginstance t1, ginstance t2)
    | On (x,i) -> On(ginstance x,i)
    | CtUnknown | _ -> raise (TypingBug "inst")

  in ginstance ct

let rec typing_expr gamma =
  let rec type_rec { e_desc = e ; e_loc = _} =
    match e with
    | Value _ -> ExpVar (new_exptype () )
    | Variable n ->begin
        try
        let sigma =  List.assoc (Ident n) gamma in
        sigma
        with  Not_found -> ExpVar (new_exptype ())
      end
    | Alternative (e1,e2,e3) ->
      let t1 = type_rec e1 in
      let t2 = type_rec e2 in
      let t3 = type_rec e3 in
      unify (t1,t2);
      unify (t2,t3); t3
    | Application (i,e2) ->
      let t2 = type_rec e2 in t2
    | ETuple t -> CTuple (List.map type_rec t)
    | InfixOp (op, e1,e2) ->
      let t1 = type_rec e1 in
      let t2 = type_rec e2 in
      unify (t1,t2) ; t1
    | PrefixOp (op, e1) -> type_rec e1
    | Unit -> CtUnknown
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
      On (t2,i)
    | Pre e -> type_rec e

  in
  type_rec

let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple pl -> List.fold_left (fun acc p -> (string_of_pattern p)@acc) [] pl



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
  | ExpVar {e_index = n ; e_value = CtUnknown } ->
    (*let name = try List.assoc n tvar_names
      with Not_found ->
        raise (TypingBug ("Non generic variable :"^(string_of_int n)))
      in *) Format.fprintf fmt "%s" (tvar_name n)
  | ExpVar {e_index = _ ; e_value = t} -> print_type fmt t
  | Arrow (t1,t2) ->
    Format.fprintf fmt "(%a -> %a)" print_type t1 print_type t2

  | CTuple tl -> Format.fprintf fmt "(%a)" (print_tuple print_type) tl
  | On (x,i) ->  Format.fprintf fmt "%a on %s"
                   print_type x
                   i;
  | CtUnknown | _ -> raise (TypingBug "print_type_scheme")


let print_type_scheme fmt (Forall(gv,t)) =

  let rec print_rec = function
    | ExpVar {e_index = n ; e_value = CtUnknown } ->
      (*let name = try List.assoc n tvar_names
        with Not_found ->
          raise (TypingBug ("Non generic variable :"^(string_of_int n)))
        in *) print_string (tvar_name n)
    | ExpVar {e_index = _ ; e_value = t} -> print_rec t
    | Arrow (t1,t2) -> print_string "("; print_rec t1;
      print_string " -> "; print_rec t2;
      print_string ")"
    | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_type) tl
    | On (x,i) ->  print_rec x; print_string " on "; print_string i;
    | CtUnknown | _ -> raise (TypingBug "print_type_scheme")
  in print_rec t



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
  typing_env := (p.p_desc,tau) :: !typing_env


let print_env fmt gamma =
  List.iter (fun (x,t) -> Format.fprintf Format.std_formatter "%a :: " print_pattern {p_desc = x ; p_loc = Location.none } ; (print_type fmt t); print_endline "") gamma

let typing eq =
  Format.fprintf Format.std_formatter "I try to clock the equation : %a \n" print_equation eq;
  reset_exptypes ();
  reset_exptypes ();

  print_env Format.std_formatter !typing_env

let tuple_of_list l = CTuple l

(* let%node f x y = (y=x) *)

let type_node node =
  typing_env := [];
  reset_exptypes ();

  let cpt = ref 1 in
  List.iter (fun p ->
      let pdsc = p.p_desc in
      let exp = ExpVar { e_index = !cpt; e_value = CtUnknown } in
      incr cpt;
      typing_env := (pdsc,exp) :: !typing_env ) node.inputs;


  print_newline ();
  List.iter (fun e -> typing_equation e) node.equations;

  let lin = List.map (fun x -> List.assoc x.p_desc !typing_env) node.inputs in
  let lout = List.map (fun x -> List.assoc x.p_desc !typing_env) node.outputs in
  let tt = Arrow (CTuple lin, CTuple lout) in
  Format.fprintf Format.std_formatter "The clock type of the node %s is %a "
    (List.hd (string_of_pattern node.name))
    print_type tt;

    print_newline ()
