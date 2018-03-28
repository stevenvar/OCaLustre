open Clocking_ast
open Imperative_ast2
open Parsing_ast
open Tools



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

let rec compile_expression_init i e =
  let ce = compile_expression_init i in
  match e.ce_desc with
  | CValue v -> IValue v
  | CVariable s -> IVariable s
  | CApplication (i,num, e) -> IApplication(i,num,ce e)
  | CCondact (b,i,e') -> ICondact(b,i,ce e')
  | CCall e -> ICall e
  | CInfixOp (op,e1,e2) -> IInfixOp(compile_infop op, ce e1, ce e2)
  | CPrefixOp (op, e) -> IPrefixOp (compile_preop op, ce e)
  | CAlternative (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
  | CUnit -> IUnit
  | CPre _ ->
    (* Error.syntax_error e.ce_loc "A pre cannot be on the left side of an equation" *)
    IRef("pre_"^i)
  | CArrow (e1,e2) -> ce e1
  | CFby (v,e') -> ce v
  | CWhen (e',i) -> ce e'
    (* IAlternative (ce i, ce e', IValue Nil) *)
  | CWhennot (e',i) -> ce e'
  (* IAlternative (ce i, IValue Nil, ce e') *)
  | CETuple el ->
    let iel = List.map (fun e -> ce e) el in
    IETuple (iel)
  | CMerge (e1,e2,e3) -> IAlternative (ce e1, ce e2, ce e3)
  | _ -> failwith "todo"

let rec compile_expression_step i e =
  let ce = compile_expression_step i in
  match e.ce_desc with
  | CValue v -> IValue v
  | CVariable s -> IVariable s
  | CApplication (i,num,e') -> IApplication(i,num,ce e')
  | CCondact (b,i,e') -> ICondact(b,i,ce e')
  | CCall e -> ICall e
  | CInfixOp (op,e1,e2) ->
    IInfixOp(compile_infop op, ce e1, ce e2)
  | CPrefixOp (op, e) ->
    IPrefixOp (compile_preop op, ce e)
  | CAlternative (e1,e2,e3) ->
    IAlternative (ce e1, ce e2, ce e3)
  | CUnit -> IUnit
  | CPre _ -> IRef ("pre_"^i)
  | CArrow (_,e2) -> ce e2
  | CFby (v,e') -> IRef (i^"_fby")
  | CWhen (e',i) -> ce e'
    (* IAlternative (ce i, ce e', IValue Nil) *)
  | CWhennot (e',i) ->
    ce e'
    (* IAlternative (ce i, IValue Nil, ce e') *)
  | CETuple el ->
    let iel = List.map (fun e -> ce e) el in
    IETuple (iel)
  | CMerge (e1,e2,e3) ->
    IAlternative (ce e1, ce e2, ce e3)
  | _ -> failwith "todo"

let compile_equation_init eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_init  (get_ident eq.cpattern) eq.cexpression}

let compile_equation_step eq =
  { i_pattern = eq.cpattern;
    i_expression = compile_expression_step (get_ident eq.cpattern) eq.cexpression }

let get_updates_init eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (e1,e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                        i_expression = compile_expression_init  (get_ident eq.cpattern) e1; }::l
     | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_updates_step eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (e1,e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                       i_expression = compile_expression_step  (get_ident eq.cpattern) e2; }::l
    | CPre e -> { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
                  i_expression = compile_expression_step (get_ident eq.cpattern) e}::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_init_fby eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CFby (e1,e2) -> { i_pattern = suffix_pattern ~suf:"_fby" eq.cpattern;
                        i_expression = IRefDef(compile_expression_step (get_ident eq.cpattern) e1 ) }::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux x acc) [] eqs

let get_app_inits eqs =
   let rec aux (p,e) l =
     match e.ce_desc with
     | CCondact(b,i,e) -> aux (p,e) l
     | CApplication (i,num,e) ->
       let app = { p with p_desc = Ident (i^(string_of_int num)^"_app")} in
       { i_pattern = app ;
         i_expression =
           IApplication_init (i,IUnit)}::l
    | _ -> l
  in
  List.fold_left (fun acc x -> aux (x.cpattern,x.cexpression) acc) [] eqs

let get_init_pres eqs =
  let aux eq l =
    match eq.cexpression.ce_desc with
    | CPre e -> { i_pattern = prefix_pattern ~pre:"pre_" eq.cpattern;
                  i_expression = IRefDef(compile_expression_init  (get_ident eq.cpattern) e)}::l
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
