open Clocking_ast
open Imperative_ast2
open Parsing_ast
open Tools

let rec get_condition c =
  let conds = Clocking.extract_conds c in
  conds
  (* let open Carriers in
   * match c with
   * | Carrier(s,c) -> get_condition c
   * | On (c, ck) -> (true,string_of_carrier ck)::get_condition c
   * | Onnot (c, ck) -> (false,string_of_carrier ck)::get_condition c
   * | CTuple cl -> List.fold_left (fun acc c -> (get_condition c)@acc) [] cl
   * | Var { value = Unknown } -> []
   * | Var { value = t } -> get_condition t
   * | _ ->
   *   let s = Format.asprintf "get_condition : %a" Clocks.print_clock (c,[]) in
   *   failwith s *)

let compile_preop op =
  match op with
  | Not -> INot
  | Neg -> INeg
  | Negf -> INegf

let compile_infop op =
  match op with
  | Equals -> IEquals
  | Plus -> IPlus
  | Minus -> IMinus
  | Times -> ITimes
  | Div -> IDiv
  | Diff -> IDiff
  | Plusf -> IPlusf
  | Minusf -> IMinusf
  | Timesf -> ITimesf
  | Divf -> IDivf
  | Inf -> IInf
  | Infe -> IInfe
  | Sup -> ISup
  | Supe -> ISupe
  | Bor -> IOr    | Band -> IAnd
  | Mod -> IMod

let rec compile_expression_init i e c =
  let ce = fun e -> compile_expression_init i e c in
  let rec compile_desc d =
  match d with
  | Value v -> IValue v
  | Variable s -> IVariable s
  | Application (i,num, e) -> IApplication(get_condition c,i,num,ce e)
  (* | Condact (l,e') -> ICondact(l,ce e') *)
  | Call e -> ICall e
  | InfixOp (op,e1,e2) -> IInfixOp(compile_infop op, ce e1, ce e2)
  | PrefixOp (op, e) -> IPrefixOp (compile_preop op, ce e)
  | Alternative (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
  | Unit -> IUnit
  | Pre _ ->
    (* Error.syntax_error e.ce_loc
       "A pre cannot be on the left side of an equation" *)
    IRef("pre_"^i)
  | Arrow (e1,e2) -> (ce e1).i_desc
  | Fby (v,e') -> (ce v).i_desc
  | When (e',i) -> (ce e').i_desc
    (* IAlternative (ce i, ce e', IValue Nil) *)
  | Whennot (e',i) -> (ce e').i_desc
  (* IAlternative (ce i, IValue Nil, ce e') *)
  | ETuple el ->
    let iel = List.map (fun e -> ce e) el in
    IETuple (iel)
  | Merge (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
  | _ -> failwith "todo"
  in
  { i_desc = compile_desc e.e_desc ; i_loc = e.e_loc }

let rec compile_expression_step i e c =
  let ce = fun e -> compile_expression_step i e c in
  let rec compile_desc d =
  match d with
  | Value v -> IValue v
  | Variable s -> IVariable s
  | Application (i,num,e') ->
    let conds = get_condition c in
    IApplication(conds,i,num,ce e')
  | Call e -> ICall e
  | InfixOp (op,e1,e2) ->
    IInfixOp(compile_infop op, ce e1, ce e2)
  | PrefixOp (op, e) ->
    IPrefixOp (compile_preop op, ce e)
  | Alternative (e1,e2,e3) ->
    IAlternative (ce e1, ce e2, ce e3)
  | Unit -> IUnit
  | Pre _ -> IRef ("pre_"^i)
  | Arrow (_,e2) -> (ce e2).i_desc
  | Fby (v,e') -> IRef (i^"_fby")
  | When (e',i) -> (ce e').i_desc
    (* IAlternative (ce i, ce e', IValue Nil) *)
  | Whennot (e',i) ->
    (ce e').i_desc
    (* IAlternative (ce i, IValue Nil, ce e') *)
  | ETuple el ->
    let iel = List.map (fun e -> ce e) el in
    IETuple (iel)
  | Merge (e1,e2,e3) ->
    IAlternative (ce e1, ce e2, ce e3)
  | Array el -> IArray (List.map (fun e -> ce e) el)
  | Array_fold (e,f,acc) ->
    IArray_fold (ce e, f, ce acc)
  | Array_map (e,f) ->
    IArray_map (ce e, f)
  | Imperative_update (e,pe) ->
    let pe = List.map (fun (e1,e2) -> ce e1, ce e2) pe in
    IImperative_update (ce e,pe )
  | Array_get (e,e') ->
    IArray_get(ce e, ce e')
  | Clock e -> failwith "todo"
      in
      { i_desc = compile_desc e.e_desc ; i_loc = e.e_loc }

let compile_equation_init eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_init
        (get_ident eq.cpattern) eq.cexpression eq.cclock;
    i_condition = [] }

let compile_equation_step eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_step
        (get_ident eq.cpattern) eq.cexpression eq.cclock;
    i_condition = [] }

let get_updates_init eqs =
  let aux eq l =
    match eq.cexpression.e_desc with
    | Fby (e1,e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                        i_expression = compile_expression_init
                            (get_ident eq.cpattern) e1 eq.cclock;
                      i_condition = [] }::l

     | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_updates_step eqs =
  let aux eq l =
    match eq.cexpression.e_desc with
    | Fby (e1,e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                        i_expression = compile_expression_step
                            (get_ident eq.cpattern) e2 eq.cclock;
                        i_condition = get_condition eq.cclock }::l
    | Pre e -> { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
                  i_expression = compile_expression_step
                      (get_ident eq.cpattern) e eq.cclock;
                  i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_init_fby eqs =
  let aux eq l =
    match eq.cexpression.e_desc with
    | Fby (e1,e2) ->
      let desc = IRefDef(compile_expression_step (get_ident eq.cpattern) e1 eq.cclock) in
      { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                        i_expression = { i_desc = desc; i_loc = e1.e_loc};
                      i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_app_inits eqs =
   let rec aux (p,e) l =
     match e.e_desc with
     (* | Condact(k,e) -> aux (p,e) l *)
    | Application (i,num,e) ->
       let app = { p with p_desc = Ident (i^(string_of_int num)^"_app")} in
       { i_pattern = app ;
         i_expression =
           { i_desc = IApplication_init (i,{i_desc = IUnit; i_loc = e.e_loc});
             i_loc = e.e_loc};
         i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux (x.cpattern,x.cexpression) acc) [] eqs

let get_init_pres eqs =
  let aux eq l =
    match eq.cexpression.e_desc with
    | Pre e ->  let desc = IRefDef(compile_expression_step (get_ident eq.cpattern) e eq.cclock) in
      { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
                  i_expression = { i_desc = desc ; i_loc = e.e_loc };
                  i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let compile_cnode cnode =
  let equations_step = List.map compile_equation_step cnode.cequations in
  let fby_updates_step = get_updates_step cnode.cequations in
  let init_fby = get_init_fby cnode.cequations in
  let init_apps = get_app_inits cnode.cequations in
  { i_name = cnode.cname;
    i_inputs = cnode.cinputs;
    i_outputs = cnode.coutputs;
    i_init = {
      i_init_apps = init_apps;
      i_init_fby = init_fby;
    };
    i_step = {
      i_step_equations = equations_step;
      i_step_updates = fby_updates_step
    }
  }
