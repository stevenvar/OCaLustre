type typvar = { index : int; mutable value : typ option }
and typ_scheme = int list * typ

and typ =
  | TUnit
  | TInt
  | TBool
  | TFloat
  | TArrow of typ * typ
  | TTuple of typ list
  | Typvariable of typvar
open Parsing_ast

type texpression = { te_desc : exp_desc ; te_typ : typ }

(** Printing **)

(* Printing variables  *)
let tvar_name n =
  let rec name_of n =
    let q,r = (n/26), (n mod 26) in
    let s = String.make 1 (char_of_int (96+r)) in
    if q = 0 then s else (name_of q)^s
  in "'"^(name_of n)

let rec print_typ fmt c =
  let rec print_tuple fmt cs =
    match cs with
    | [] -> ()
    | [c] -> Format.fprintf fmt  "%a" print_typ c
    | c::s -> Format.fprintf fmt "%a * %a" print_typ c print_tuple s
  in
  match c with
  | TArrow (c1,c2) ->
    Format.fprintf fmt " (%a -> %a) " print_typ c1 print_typ c2
  | Typvariable { index = n; value = None } ->
    Format.fprintf fmt "'%d" n
  | Typvariable { index = n; value = Some c } ->
    Format.fprintf fmt "%a" print_typ c
  | TTuple (cs) -> Format.fprintf fmt "(%a)" print_tuple cs
  | TUnit -> Format.fprintf fmt "unit"
  | TInt -> Format.fprintf fmt "int"
  | TBool -> Format.fprintf fmt "bool"
  | TFloat -> Format.fprintf fmt "float"

let rec print_typ_scheme fmt (gv,c) =
 let names =
    let rec names_of = function
      | (n,[]) -> []
      | (n, (_::xs)) -> (tvar_name n)::(names_of (n+1,xs))
    in
    names_of (1,gv) in
  let cvar_names = List.combine (List.rev gv) names in
  let rec print_tuple fmt cs =
    match cs with
    | [] -> ()
    | [c] -> Format.fprintf fmt  "%a" print_typ_scheme (gv,c)
    | c::s -> Format.fprintf fmt "%a * %a" print_typ_scheme (gv,c) print_tuple s
  in
  match c with
  | TArrow (c1,c2) ->
    Format.fprintf fmt " (%a -> %a) " print_typ_scheme (gv,c1)
      print_typ_scheme (gv,c2)
  | Typvariable { index = n; value = None } ->
      let name =
        (try List.assoc n cvar_names
         with  Not_found -> failwith ("not found"^(string_of_int n)))
      in
      Format.fprintf fmt "%s" name
  | Typvariable { index = n; value = Some c } ->
    Format.fprintf fmt "%a" print_typ_scheme (gv,c)
  | TTuple (cs) -> Format.fprintf fmt "(%a)" print_tuple cs
  | TUnit -> Format.fprintf fmt "unit"
  | TInt -> Format.fprintf fmt "int"
  | TBool -> Format.fprintf fmt "bool"
  | TFloat -> Format.fprintf fmt "float"

(** Variables **)

let new_vartyp, reset_vartyp =
  let cpt = ref 0 in
  (fun () ->
     incr cpt;
     Typvariable { index = !cpt ; value = None }),
  (fun () -> cpt := 0)

(** Shorten **)

exception Occurs


let occurs { index = n; value = _} =
  let rec occrec = function
    | Typvariable { index = m; value = _} -> if n = m then raise Occurs
    | TArrow (c1,c2) -> occrec c1 ; occrec c2
    | TTuple cs -> List.iter (fun t -> occrec t) cs
    | TInt -> ()
    | TBool -> ()
    | TFloat -> ()
    | TUnit -> ()
  in
  occrec


let rec shorten t =
  (* Format.printf "Shorten %a \n" print_typ t; *)
  match t with
  | TArrow (t1,t2) -> TArrow(shorten t1, shorten t2)
  | TTuple tls -> TTuple (List.map shorten tls)
  | Typvariable { index = _; value = None} -> t
  | Typvariable { index = _;
                   value = Some (Typvariable ({ index = _;
                                         value = None }) as tv)} -> tv
  | Typvariable ({ index = _; value = Some (Typvariable tv1) } as tv2) ->
    tv2.value <- tv1.value;
    shorten t
  | Typvariable { index = _ ; value = Some t' } -> shorten t'
  | _ -> t

(** Unify **)

exception Unify of typ * typ

let rec unify t1 t2 =
  let t1 = shorten t1 in
  let t2 = shorten t2 in
  (* Format.printf "Unify %a and %a \n" print_typ c1 print_typ c2; *)
  try
    (match t1, t2 with
     | TUnit, TUnit -> ()
     | TInt, TInt -> ()
     | TBool, TBool -> ()
     | TFloat, TFloat -> ()
     | Typvariable ({ index = n ; value = None } as tv1),
       Typvariable ({ index = m; value = None } as tv2) ->
       if n <> m then tv1.value <- Some t2
     | Typvariable ({ index = n ; value = None } as tv), c2 ->
       occurs tv c2;
       tv.value <- Some c2
     | c1 , Typvariable ({ index = n ; value = None } as tv) ->
       occurs tv c1;
       tv.value <- Some c1
     | TArrow (c1,c2), TArrow (c3,c4) ->
       unify c1 c3;
       unify c2 c4
     | TTuple cs, TTuple vs ->
       (try
          if List.length cs <> List.length vs then raise (Invalid_argument "length");
         List.iter2 (fun a b -> unify a b) cs vs
       with Invalid_argument _ -> raise (Unify(t1,t2)))
     | _ -> raise (Unify (t1,t2)))
  with Occurs -> raise (Unify (t1,t2))


(** Instantiation **)



let gen_instance (gv,tau) =
  let gv_unknowns = List.map (fun n -> n, new_vartyp ()) gv in
  let rec ginstance t =
    match t with
    | Typvariable { index = n; value = None}->
      begin try
          List.assoc n gv_unknowns
        with Not_found -> t
      end
    | Typvariable { index = _; value = Some t } -> ginstance t
    | TArrow (c1,c2) -> TArrow(ginstance c1, ginstance c2)
    | TTuple cs -> TTuple (List.map ginstance cs)
    | TUnit | TInt | TBool | TFloat -> t
  in
  ginstance tau

(** Generalization **)

(* returns typ vars, stream vars, carrier vars *)
let vars_of_typ tau =
  let rec vars vs v =
    match v with
    | TArrow (c1,c2) ->
      let vs = vars vs c1 in
      let vs = vars vs c2 in
      vs
    | Typvariable { index = n; value = None} ->
      if List.mem n vs then vs else (n::vs)
    | Typvariable { index = n; value = Some t } -> vars vs t
    | TTuple [] -> vs
    | TTuple (x::xs) ->
      let vs = vars vs x in
      List.fold_left (fun vs x ->
          let v = vars vs x in v@vs) vs xs
    | TUnit | TInt | TBool | TFloat -> vs
  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_typ bv t =
  let vs  = vars_of_typ t in
  substract vs bv

let unknowns_of_typ_env env =
  let vs = List.fold_left (fun vs (gv,t) ->
      let v = unknowns_of_typ gv t in
      (v::vs)) [] env in
  List.flatten vs

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalize_typ gamma tau =
  let vs = vars_of_typ tau in
  let uv = unknowns_of_typ_env gamma in
  let genvars = make_set (substract vs uv) in
  genvars, tau

(** Type inference **)

let rec typ_expr gamma e =
      match e.e_desc with
      | Unit -> TUnit
      | Call _ -> new_vartyp ()
      | Value Nil -> new_vartyp ()
      | Value (Integer _) ->
        TInt
      | Value (Bool _) ->
        TBool
      | Value (Float _) ->
        TFloat
      | Variable n ->
        let sigma = try List.assoc n gamma
          with Not_found ->
            let s = Format.asprintf "Unbound variable : %s" n in
            Error.print_error e.e_loc s
        in
        gen_instance sigma
      | PrefixOp (Not,e) ->
        let t = typ_expr gamma e in
        unify t TBool;
        t
      | PrefixOp (Neg,e) ->
        let t = typ_expr gamma e in
        unify t TInt;
        t
      | PrefixOp (Negf,e) ->
        let t = typ_expr gamma e in
        unify t TFloat;
        t
      | InfixOp ((Plus | Minus | Times | Div | Mod),e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        unify t1 TInt;
        t1
      | InfixOp ((Plusf | Minusf | Timesf | Divf),e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        unify t1 TFloat;
        t1
      | InfixOp ((Bor | Band ),e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        unify t1 TBool;
        t1
      | InfixOp ((Diff | Equals | Sup | Supe | Inf | Infe) ,e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        TBool
      | Alternative (e1,e2,e3) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        let t3 = typ_expr gamma e3 in
        unify t1 TBool;
        unify t2 t3;
        t2
      | Fby (e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        t1
      | Pre e -> typ_expr gamma e
      | Arrow (e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t1 t2;
        t1
      | ETuple es ->
        let cs = List.map (typ_expr gamma) es in
        TTuple cs
      | When (e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t2 TBool;
        t1
      | Application (id,num,e) ->
        begin
          let fun_typ = typ_expr gamma { e_desc = Variable id;
                                             e_loc = Location.none} in
          let e = typ_expr gamma e in
          let u = new_vartyp () in
          let t = TArrow(e,u) in
          unify t fun_typ;
          u
        end
      | Whennot (e1,e2) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        unify t2 TBool;
        t1
      | Merge (e1,e2,e3) ->
        let t1 = typ_expr gamma e1 in
        let t2 = typ_expr gamma e2 in
        let t3 = typ_expr gamma e3 in
        unify t1 TBool;
        unify t2 t3;
        t2
      | _ ->
        let s = Format.asprintf "%a : todo" Parsing_ast_printer.print_expression e in
        Error.print_error e.e_loc s

(** Typing **)


let print_env env =
  let rec print_vars fmt gv =
    match gv with
    | [] -> ()
    | [v] -> Format.fprintf fmt "%d" v
    | v::vs -> Format.fprintf fmt "%d,%a" v print_vars vs
  in
  List.iter (fun (x,(gv,y)) ->
      Format.printf "\t forall %a . %s  => %a \n"
        print_vars gv
        x
        print_typ y) env

type tequation = { tpattern : Parsing_ast.pattern ; texpression : texpression }

let rec lookup env p =
   match p.p_desc with
  | Ident i ->
     (try
        List.assoc i env
      with Not_found ->
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
     let typs = List.map (lookup env) t in
     let clk = TTuple typs in clk
  | PUnit -> failwith "unit"
  | Typed (p,t) -> lookup env p

let remove_typs p =
  match p.p_desc with
  | Typed(p,s) -> p
  | _ -> p


let split_tuple p =
  match p.p_desc with
  | Tuple pl -> List.map remove_typs pl
  | _ -> [remove_typs p]

let group_tuple pl =
  match pl with
  | [] -> failwith "group tuple"
  | [x] -> x
  | _ -> TTuple pl


let rec assoc_env (env:(string * typ_scheme) list) p =
   match p.p_desc with
  | Ident i ->
     (try
        List.assoc i env
      with Not_found ->
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
    let typs = List.map (assoc_env env) t in
    let typs = List.map (gen_instance) typs in
    (* let clk = TTuple typs in generalize_typ env clk *)
    ([],TTuple typs)
  | PUnit -> failwith "unit"
  | Typed (p,t) -> assoc_env env p


let typ_equations gamma eqs =
  let rec typ_eq gamma eq =
    (try
       let clk = typ_expr gamma eq.expression in
       let pck = assoc_env gamma eq.pattern in
       let pck = gen_instance pck in
       unify pck clk;
       { tpattern = eq.pattern ;
         texpression = { te_desc = eq.expression.e_desc; te_typ = clk}
       }
     with Unify (c1,c2) ->
       (let s = Format.asprintf "Type clash between %a and %a"
            print_typ_scheme (unknowns_of_typ [] c1, c1)
            print_typ_scheme (unknowns_of_typ [] c2, c2)
        in
        Error.print_error eq.expression.e_loc s));

  in
  List.map (typ_eq gamma) eqs


let get_all_vars node =
  let vars =
    List.map (fun eq -> split_tuple eq.pattern) node.equations
  in
  let vars = List.flatten vars in
  (* let vars = [] in *)
  let ins = split_tuple node.inputs in
  (* let outs = split_tuple node.outputs in *)
  Tools.make_set (vars@ins)

let rec lookup_typ env p =
  let s = Tools.string_of_pattern p in
  try
    List.assoc s env
  with Not_found ->
    Error.print_error p.p_loc ("Unbound variable "^s)

let typ_node gamma node types =
  reset_vartyp ();
  let vars = get_all_vars node in
  let vars = List.map Tools.string_of_pattern vars in
  let vars_typs =
    List.map (fun x ->if x = "()" then (x,([],TUnit)) else (x,([],new_vartyp()))) vars in
  let env = vars_typs@gamma in
  let eqs = typ_equations env node.equations in
  let ckins = List.map (fun x -> lookup_typ env x)
      (split_tuple node.inputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map (fun x -> lookup_typ env x)
      (split_tuple node.outputs) in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_typ = TArrow(ckins,ckouts) in
  (* print_env env; *)
  let node_typ_scheme = generalize_typ [] node_typ in
  if types then
  Format.printf "\n%a : %a \n"
    Parsing_ast_printer.print_pattern node.name
    print_typ_scheme node_typ_scheme;
  ((Tools.string_of_pattern node.name,node_typ_scheme))::gamma
