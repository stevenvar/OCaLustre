open Clocking_ast
open Imperative_ast
open Parsing_ast
open Tools



let rec extract_conds c =
  begin
    match Clocking.shorten_ck c with
    | Ckon (x,c) ->
      (true,c)::(extract_conds x)
    | Ckonnot (x,c) ->
      (false,c)::(extract_conds x)
    | _ -> []
  end

let rec get_condition c =
  match c with
  | Ck c -> extract_conds c
  | CkTuple cs -> List.fold_left (fun acc x -> acc@get_condition x)
                    []
                    cs

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
  let compile_desc d =
    match d with
    | CValue v -> IValue v
    | CVariable s -> IVariable s
    | CApplication (i,num,c, e) -> IApplication(get_condition (Ck c),i,num,ce e)
    (* | Condact (l,e') -> ICondact(l,ce e') *)
    | CCall (f,e) -> ICall (f, ce e)
    | CInfixOp (op,e1,e2) -> IInfixOp(compile_infop op, ce e1, ce e2)
    | CPrefixOp (op, e) -> IPrefixOp (compile_preop op, ce e)
    | CAlternative (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
    | CUnit -> IUnit
    | CPre _ ->
      (* Error.syntax_error e.ce_loc
         "A pre cannot be on the left side of an equation" *)
      IRef("pre_"^i)
    | CArrow (e1,_e2) -> (ce e1).i_desc
    | CFby (v,_e') -> (ce v).i_desc
    | CWhen (e',_i) -> (ce e').i_desc
    (* IAlternative (ce i, ce e', IValue Nil) *)
    | CWhennot (e',_i) -> (ce e').i_desc
    (* IAlternative (ce i, IValue Nil, ce e') *)
    | CETuple el ->
      let iel = List.map (fun e -> ce e) el in
      IETuple (iel)
    | CMerge (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
    | _ -> failwith "todo"
  in
  { i_desc = compile_desc e.ce_desc ; i_loc = e.ce_loc }

let rec compile_expression_step i e c =
  let ce = fun e -> compile_expression_step i e c in
  let compile_desc d =
    match d with
    | CValue v -> IValue v
    | CVariable s -> IVariable s
    | CApplication (i,num,c,e') ->
      let conds = get_condition (Ck c) in
      IApplication(conds,i,num,ce e')
    | CCall (f,e) -> ICall (f, ce e)
    | CInfixOp (op,e1,e2) ->
      IInfixOp(compile_infop op, ce e1, ce e2)
    | CPrefixOp (op, e) ->
      IPrefixOp (compile_preop op, ce e)
    | CAlternative (e1,e2,e3) ->
      IAlternative (ce e1, ce e2, ce e3)
    | CUnit -> IUnit
    | CPre _ -> IRef ("pre_"^i)
    | CArrow (_,e2) -> (ce e2).i_desc
    | CFby _ -> IRef (i^"_fby")
    | CWhen (e',_i) -> (ce e').i_desc
    (* IAlternative (ce i, ce e', IValue Nil) *)
    | CWhennot (e',_i) ->
      (ce e').i_desc
    (* IAlternative (ce i, IValue Nil, ce e') *)
    | CETuple el ->
      let iel = List.map (fun e -> ce e) el in
      IETuple (iel)
    | CMerge (e1,e2,e3) ->
      IAlternative (ce e1, ce e2, ce e3)
    | CArray el -> IArray (List.map (fun e -> ce e) el)
    | CArray_fold (e,f,acc) ->
      IArray_fold (ce e, f, ce acc)
    | CArray_map (e,f) ->
      IArray_map (ce e, f)
    | CImperative_update (e,pe) ->
      let pe = List.map (fun (e1,e2) -> ce e1, ce e2) pe in
      IImperative_update (ce e,pe )
    | CArray_get (e,e') ->
      IArray_get(ce e, ce e')
    | _ -> failwith "todo"
  in
  { i_desc = compile_desc e.ce_desc ; i_loc = e.ce_loc }

let compile_equation_init eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_init
        (get_ident eq.cpattern)
        eq.cexpression
        eq.cclock;
    i_condition = [] }

let compile_equation_step eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_step
        (get_ident eq.cpattern)
        eq.cexpression
        eq.cclock;
    i_condition = [] }

let get_updates_init eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (e1,_e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                         i_expression = compile_expression_init
                             (get_ident eq.cpattern) e1 eq.cclock;
                         i_condition = [] }::l

    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_updates_step eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (_e1,e2) ->
      let cond = get_condition eq.cclock in
      { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
        i_expression = compile_expression_step
            (get_ident eq.cpattern) e2 eq.cclock;
        i_condition = cond }::l
    | CPre e -> { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
                  i_expression = compile_expression_step
                      (get_ident eq.cpattern) e eq.cclock;
                  i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_init_fby eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (e1,_e2) ->
      let desc = IRefDef(compile_expression_step (get_ident eq.cpattern) e1 eq.cclock) in
      { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
        i_expression = { i_desc = desc; i_loc = e1.ce_loc};
        i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_app_inits eqs =
  let aux (p,e) l =
    match e.ce_desc with
    (* | Condact(k,e) -> aux (p,e) l *)
    | CApplication (i,num,_c,e) ->
      let app = { p with p_desc = Ident (i^(string_of_int num)^"_app")} in
      { i_pattern = app ;
        i_expression =
          { i_desc = IApplication_init (i,{i_desc = IUnit; i_loc = e.ce_loc});
            i_loc = e.ce_loc};
        i_condition = [] }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux (x.cpattern,x.cexpression) acc) [] eqs

let get_init_pres eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CPre e ->  let desc = IRefDef(compile_expression_step (get_ident eq.cpattern) e eq.cclock) in
      { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
        i_expression = { i_desc = desc ; i_loc = e.ce_loc };
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
