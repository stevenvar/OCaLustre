type clkvar = { index : int; mutable value : clock }
and sckvar = { s_index : int; mutable s_value : stream_clock }
and cvar = { c_index : int; mutable c_value : carrier }
and clock_scheme = int list * int list * int list * clock

and carrier =
  | CarUnknown
  | N of string
  | X of cvar

and clock =
  | ClkUnknown
  | ClkArrow of clock * clock
  | ClkTuple of clock list
  | ClkVariable of clkvar
  | ClkStream of stream_clock
  | ClkDep of carrier * stream_clock

and stream_clock =
  | SckUnknown
  | SckBase
  | SckOn of stream_clock * carrier
  | SckOnnot of stream_clock * carrier
  | SckVariable of sckvar

open Parsing_ast

(* type expr =
 *   | Const
 *   | Var of string
 *   | Op of expr * expr
 *   | When of expr * expr
 *   | Whennot of expr * expr
 *   | Merge of expr * expr * expr
 *   | Clock of expr
 *   | App of expr * expr
 *
 * type expression = { e_desc : expr } *)
type cexpression = { ce_desc : exp_desc ; ce_clock : clock }

(** Printing **)

let rec print_car fmt c =
  match c with
  | CarUnknown -> Format.fprintf fmt "'c"
  | N x -> Format.fprintf fmt "%s" x
  | X { c_index = n ; c_value = CarUnknown } -> Format.fprintf fmt "'cl_%d" n
  | X { c_index = _ ; c_value = c } -> Format.fprintf fmt "*%a" print_car c

let rec print_stream fmt s =
  match s with
  | SckUnknown -> Format.fprintf fmt "'s"
  | SckBase -> Format.fprintf fmt "base"
  | SckOn (x,c) -> Format.fprintf fmt "(%a on %a)" print_stream x print_car c
  | SckOnnot (x,c) -> Format.fprintf fmt "(%a on not %a)" print_stream x print_car c
  | SckVariable { s_index = n; s_value = SckUnknown } -> Format.fprintf fmt "'s_%d" n
  | SckVariable { s_index = n; s_value = s } -> Format.fprintf fmt "*%d%a" n print_stream s

let rec print_clock fmt c =
  let rec print_tuple fmt cs =
    match cs with
    | [] -> ()
    | [c] -> Format.fprintf fmt  "%a" print_clock c
    | c::s -> Format.fprintf fmt "%a * %a" print_clock c print_tuple s
  in
  match c with
  | ClkUnknown -> Format.fprintf fmt "'c"
  | ClkArrow (c1,c2) -> Format.fprintf fmt "%a -> %a" print_clock c1 print_clock c2
  | ClkVariable { index = n; value = ClkUnknown } -> Format.fprintf fmt "'ck_%d" n
  | ClkVariable { index = n; value = c } -> Format.fprintf fmt "*%a" print_clock c
  | ClkStream s -> Format.fprintf fmt "%a" print_stream s
  | ClkDep (car,s) -> Format.fprintf fmt "(%a : %a)" print_car car print_stream s
  | ClkTuple (cs) -> Format.fprintf fmt "(%a)" print_tuple cs

(** Variables **)

let new_varclock =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    ClkVariable { index = !cpt ; value = ClkUnknown }


let new_varstream =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    SckVariable { s_index = !cpt ; s_value = SckUnknown }


let new_varcar =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    X { c_index = !cpt ; c_value = CarUnknown }

let fresh_name =
  let cpt = ref 0 in
  fun () ->
    incr cpt;
    N ("car_"^(string_of_int !cpt))

(** Shorten **)

exception Occurs
let occurs { index = n; value = _} =
  let rec occrec = function
    | ClkVariable { index = m; value = _} -> if n = m then raise Occurs
    | ClkArrow (c1,c2) -> occrec c1 ; occrec c2
    | ClkTuple cs -> List.iter (fun t -> occrec t) cs
    | ClkUnknown -> raise Occurs
    | ClkStream s -> ()
    | ClkDep (c,s) -> ()
  in
  occrec

let rec shorten_car (c:carrier) =
  match c with
  | CarUnknown -> failwith "shorten car"
  | X { c_index = _; c_value = CarUnknown} -> c
  (* | X { c_index = _; c_value = c} -> shorten_car c *)
  | X ({ c_index = _; c_value = X tv1 } as tv2) ->
    tv2.c_value <- tv1.c_value;
    shorten_car c
  | _ -> c

let rec shorten_stream (s:stream_clock) =
  match s with
  | SckUnknown -> failwith "shorten_stream"
  | SckOn (s, car) -> SckOn (shorten_stream s, shorten_car car)
  | SckOnnot (s, car) -> SckOnnot (shorten_stream s, shorten_car car)
  | SckVariable { s_index = _; s_value = SckUnknown } -> s
  (* | SckVariable { s_index = _; s_value = v } -> shorten_stream v *)
  | SckVariable ({ s_index = _; s_value = SckVariable tv1 } as tv2) ->
    tv2.s_value <- tv1.s_value;
    shorten_stream s
  | _ -> s

let rec shorten c =
  (* Format.printf "Shorten %a \n" print_clock c; *)
  match c with
  | ClkUnknown -> failwith "shorten"
  | ClkArrow (c1,c2) -> ClkArrow(shorten c1, shorten c2)
  | ClkTuple cls -> ClkTuple (List.map shorten cls)
  | ClkVariable { index = _; value = ClkUnknown} -> c
  | ClkVariable ({ index = _;
                  value = ClkVariable { index = _; value = ClkUnknown } as tv}) -> tv
  | ClkVariable ({ index = _; value = ClkVariable tv1 } as tv2) ->
    tv2.value <- tv1.value;
    shorten c
  | ClkStream s -> ClkStream (shorten_stream s)
  | ClkDep (car, s) -> ClkDep(shorten_car car, shorten_stream s)
  | _ -> c

(** Unify **)

exception Unify of clock * clock
exception Stream_clash of stream_clock * stream_clock
exception Carrier_clash of carrier * carrier

let rec unify_carrier c1 c2 =
  let c1 = shorten_car c1 in
  let c2 = shorten_car c2 in
  Format.printf "Unify carrier %a and %a \n" print_car c1 print_car c2;
  match c1,c2 with
  | N s , N s' -> if not (s = s') then raise (Carrier_clash (c1,c2))
  | X ({ c_index = n; c_value = CarUnknown} as tv1),
    X ({ c_index = m; c_value = CarUnknown} as tv2) ->
    if n <> m then tv1.c_value <- c2
  | X ({ c_index = n; c_value = CarUnknown} as tv), c2 ->
    tv.c_value <- c2
  | c1, X ({ c_index = n; c_value = CarUnknown} as tv) ->
    tv.c_value <- c1
  | _ -> raise (Carrier_clash (c1,c2))

let rec unify_stream s1 s2 =
  let s1 = shorten_stream s1 in
  let s2 = shorten_stream s2 in
  Format.printf "Unify stream %a %a \n" print_stream s1 print_stream s2;
  let unif = match s1, s2 with
  (* | SckUnknown, SckUnknown -> () *)
  | SckBase, SckBase -> ()
  | SckVariable ({ s_index = n ; s_value = SckUnknown } as tv1),
    SckVariable ({ s_index = m ; s_value = SckUnknown } as tv2) ->
    if n <> m then tv1.s_value <- s2
  | SckVariable ({ s_index = n ; s_value = SckUnknown } as tv), s' ->
    tv.s_value <- s'
  | s , SckVariable ({ s_index = n ; s_value = SckUnknown } as tv) ->
    tv.s_value <- s
  | SckOn (s,c), SckOn(s',c') ->
    (try
       unify_carrier c c'
     with Carrier_clash _ -> raise (Stream_clash (s1,s2)));
    unify_stream s s'
  | SckOnnot (s,c), SckOnnot(s',c') ->
    (try
       unify_carrier c c'
     with Carrier_clash _ -> raise (Stream_clash (s1,s2)));
    unify_stream s s'
  | _ -> raise (Stream_clash (s1,s2))
  in
  Format.printf "After Unify stream %a %a \n" print_stream s1 print_stream s2;
    unif

let rec unify c1 c2 =
  let c1 = shorten c1 in
  let c2 = shorten c2 in
  (* Format.printf "Unify %a and %a \n" print_clock c1 print_clock c2; *)
  match c1, c2 with
  | ClkVariable ({ index = n ; value = ClkUnknown } as tv1),
    ClkVariable ({ index = m; value = ClkUnknown } as tv2) ->
    if n <> m then tv1.value <- c2
  | ClkVariable ({ index = n ; value = ClkUnknown } as tv), c2 ->
    occurs tv c2;
    tv.value <- c2
  | c1 , ClkVariable ({ index = n ; value = ClkUnknown } as tv) ->
    occurs tv c1;
    tv.value <- c1
  | ClkStream s, ClkStream s' ->
    (try
      unify_stream s s'
    with Stream_clash _ -> raise (Unify(c1,c2)))
  | ClkDep (car,s), ClkDep (car',s') ->
    (try
      unify_carrier car car'
     with Carrier_clash _ -> raise (Unify(c1,c2)));
    (try
      unify_stream s s';
     with Stream_clash _ -> raise (Unify(c1,c2)))
  | ClkArrow (c1,c2), ClkArrow (c3,c4) ->
    unify c1 c3;
    unify c2 c4
  | _ -> raise (Unify (c1,c2))


(** Instantiation **)

let gen_instance_car (gv,gs,gc,car) =
  let unknowns = List.map (fun n -> n, new_varcar ()) gc in
  let rec ginstance c =
    match c with
    | CarUnknown -> failwith "gen_instance_car"
    | N x -> c
    | X { c_index = n; c_value = CarUnknown } ->
      begin
        try List.assoc n unknowns with Not_found ->
          (
            (* Format.printf "not found : %a \n" print_car c; *)
          c)
      end
     | X { c_index = _; c_value = c } -> ginstance c
  in
  ginstance car

let gen_instance_stream (gv,gs,gc,car) =
  let unknowns = List.map (fun n -> n, new_varstream ()) gs in
  let rec ginstance s =
    match s with
    | SckUnknown -> failwith "gen_instance_stream"
    | SckBase -> s
    | SckVariable { s_index = n ; s_value = SckUnknown} ->
      begin
        try List.assoc n unknowns with Not_found ->(
          (* Format.printf "not found : %a \n" print_stream s; *)
          s)
      end
    | SckVariable { s_index = _; s_value = s } -> ginstance s
    | SckOn (s,car) -> SckOn (ginstance s,gen_instance_car (gv,gs,gc,car))
    | SckOnnot (s,car) -> SckOnnot (ginstance s, gen_instance_car (gv,gs,gc,car))
  in
  ginstance car

let gen_instance (gv,gs,gc,tau) =
  let unknowns = List.map (fun n -> n, new_varclock ()) gv in
  let rec ginstance t =
    match t with
    | ClkVariable { index = n; value = ClkUnknown}->
      begin try
          List.assoc n unknowns
        with Not_found -> t
      end
    | ClkVariable { index = _; value = t } -> ginstance t
    | ClkStream s -> ClkStream (gen_instance_stream (gv,gs,gc,s))
    | ClkArrow (c1,c2) -> ClkArrow(ginstance c1, ginstance c2)
    | ClkUnknown -> failwith "gen_instance"
    | ClkTuple cs -> ClkTuple (List.map ginstance cs)
    | ClkDep (car,s) -> ClkDep(gen_instance_car (gv,gs,gc,car),
                               gen_instance_stream (gv,gs,gc,s))
  in
  ginstance tau

(** Generalization **)

let vars_of_clock tau =
  let rec vars vs c =
    match c with
    | ClkUnknown -> failwith "vars_of_clock"
    | ClkArrow (c1,c2) -> vars (vars vs c1) c2
    | ClkVariable { index = n; value = ClkUnknown} ->
      if List.mem n vs then vs else n::vs
    | ClkVariable { index = n; value = t } -> vars vs t
    | ClkTuple [] -> []
    | ClkTuple (c::cs) -> List.fold_left (fun acc t -> vars acc t) (vars vs c) cs
    | ClkStream s -> vs
    | ClkDep (var,s) -> vs
  in vars [] tau

let substract l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let unknowns_of_clock_env (bv,t) = substract (vars_of_clock t) bv

let rec make_set l =
  match l with
  | [] -> []
  | x::l -> if List.mem x l then make_set l else x :: make_set l

let generalize_clock gamma tau =
  let genvars = make_set (substract (vars_of_clock tau) (unknowns_of_clock_env gamma)) in
  (genvars, [] (* cars *), tau)

(** Clock inference **)

let rec clock_expr gamma e =
  try
    begin
      match e.e_desc with
      | Value v ->
        new_varclock ()
        (* ClkStream (SckBase) *)
      | Variable n ->
        let sigma = try List.assoc n gamma
          with Not_found ->
            let s = Format.asprintf "Unbound variable : %s" n in
            Error.print_error e.e_loc s
        in
        let clk = gen_instance sigma in
        Format.printf "new instance of %s = %a \n" n print_clock clk;
        clk
        (* new_varclock () *)
      | InfixOp (op,e1,e2) ->
        let c1 = clock_expr gamma e1 in
        let c2 = clock_expr gamma e2 in
        unify c1 c2;
        c1
      | When (e1,e2) ->
        let c1 = clock_expr gamma e1 in
        let c2 = clock_expr gamma e2 in
        let a = new_varstream () in
        let car = new_varcar () in
        let tt = ClkArrow (ClkStream a, ClkArrow(ClkDep(car,a),ClkStream (SckOn(a,car)))) in
        let res = new_varclock () in
        let tau = ClkArrow(c1,ClkArrow(c2,res)) in
        unify tau tt;
        res
      | Whennot (e1,e2) ->
        let c1 = clock_expr gamma e1 in
        let c2 = clock_expr gamma e2 in
        let a = new_varstream () in
        let car = new_varcar () in
        let tt = ClkArrow (ClkStream a, ClkArrow(ClkDep(car,a),ClkStream (SckOnnot(a,car)))) in
        let res = new_varclock () in
        let tau = ClkArrow(c1,ClkArrow(c2,res)) in
        unify tau tt;
        res
      | Merge (e1,e2,e3) ->
        let c1 = clock_expr gamma e1 in
        let c2 = clock_expr gamma e2 in
        let c3 = clock_expr gamma e3 in
        let a = new_varstream () in
        let car = new_varcar () in
        let tt = ClkArrow (ClkDep(car, a),
                           ClkArrow(ClkStream(SckOn(a,car)),
                                    ClkArrow(ClkStream(SckOnnot(a,car)),ClkStream a))) in
        let res = new_varclock () in
        let tau = ClkArrow(c1,ClkArrow(c2,ClkArrow(c3,res))) in
        unify tau tt;
        res
      | Clock e ->
        let u = new_varstream () in
        let v = clock_expr gamma e in
        let c = fresh_name () in
        (* let c = new_varcar () in *)
        unify (ClkStream u) v;
        ClkDep(c,u)
      | _ -> failwith "todo"
    end
  with Unify (c1,c2) ->
    let s = Format.asprintf "Clock clash between %a and %a" print_clock c1 print_clock c2 in
    Error.print_error e.e_loc s

(** Check that a clock is correct (i.e no out of scope sampler) **)

let check_clock loc clk =
  let rec check_car car =
    (* Format.printf "Checking %a\n" print_car car; *)
    match car with
    | CarUnknown -> ()
    | N s ->
      let s = Format.asprintf "the sampler %s escapes its scope" s in
      Error.print_error loc s
    | X { c_index = _; c_value = c} -> check_car c
  in
  let rec check_stream s =
    (* Format.printf "Checking %a\n" print_stream s; *)
    match s with
    | SckUnknown -> ()
    | SckBase -> ()
    | SckOn (s,c) -> check_stream s; check_car c
    | SckOnnot (s,c) -> check_stream s; check_car c
    | SckVariable { s_index = _; s_value = s } -> check_stream s
  in
  let rec check clk =
    (* Format.printf "Checking %a\n" print_clock clk; *)
    match clk with
    | ClkUnknown -> ()
    | ClkArrow (c1,c2) -> check c1 ; check c2
    | ClkTuple cs -> List.iter check cs
    | ClkDep (c,s) -> check_car c; check_stream s
    | ClkStream s -> check_stream s
    | ClkVariable { index = _ ; value = c} -> check c
  in
  check clk

(** Clocking **)


let print_env env =
  List.iter (fun (x,(_,_,_,y)) -> Format.printf "\t %s -> %a \n" x print_clock y) env

type cequation = { cpattern : Parsing_ast.pattern ; cexpression : cexpression }

let rec lookup env p =
   match p.p_desc with
  | Ident i ->
     (try
        List.assoc i env
      with Not_found ->
        Error.print_error p.p_loc ("Not found : "^i))
  | Tuple t ->
     let clocks = List.map (lookup env) t in
     let clk = ClkTuple clocks in clk
  | PUnit -> failwith "unit"
  | Typed (p,t) -> lookup env p

let remove_types p =
  match p.p_desc with
  | Typed(p,s) -> p
  | _ -> p


let split_tuple p =
  match p.p_desc with
  | Tuple pl -> List.map remove_types pl
  | _ -> [remove_types p]

let group_tuple pl =
  match pl with
  | [] -> ClkUnknown
  | [x] -> x
  | _ -> ClkTuple pl

let clock_equations gamma eqs =
  let rec clock_eq gamma eq =
    (* Format.printf "clock_expr %a \n" Parsing_ast_printer.print_expression eq.expression; *)
    let clk = clock_expr gamma eq.expression in
    let gamma' = List.map (fun (x,y) -> x, gen_instance y) gamma in
    let pck = lookup gamma' eq.pattern in
    unify pck clk;
    gamma,{ cpattern = eq.pattern ;
            cexpression = { ce_desc = eq.expression.e_desc; ce_clock = clk}
          }
  in
  List.fold_left (fun (env,acc) e ->
      let (new_env,e') = clock_eq (List.rev env) e in
      (new_env,e'::acc)) (gamma,[]) eqs

let clock_node node =
  let vars = Clocking.get_all_vars node in
  let vars = List.map Clocks.string_of_pat vars in
  let vars_clocks =
    List.map (fun x -> (x,([],[],[],new_varclock()))) vars in
  let env = vars_clocks in
  print_env env;
  let (env,eqs) = clock_equations env node.equations in
  print_env env;
  let ckins = List.map (fun x -> Clocks.lookup_clock env x) (split_tuple node.inputs) in
  let ckins = List.map gen_instance ckins in
  let ckins = group_tuple ckins in
  let ckouts = List.map (fun x -> Clocks.lookup_clock env x) (split_tuple node.outputs) in
  let ckouts = List.map gen_instance ckouts in
  let ckouts = group_tuple ckouts in
  let node_clock = ClkArrow(ckins,ckouts) in
  Format.printf "%a :: %a \n" Parsing_ast_printer.print_pattern node.name print_clock node_clock;
  check_clock node.name.p_loc node_clock;
  (* List.iter (fun { cpattern; cexpression } -> *)
      (* Format.printf "%a :: %a\n" *)
        (* Parsing_ast_printer.print_pattern cpattern *)
        (* print_clock cexpression.ce_clock) eqs *)
