open Parsing_ast
open Clocking_ast
open Sequential_ast
open Tools

let seq_preop op =
  match op with
  | Not -> S_Not
  | Neg -> S_Neg
  | Negf -> S_Negf

let seq_infop op =
  match op with
  | Equals -> S_Equals
  | Plus -> S_Plus
  | Minus -> S_Minus
  | Times -> S_Times
  | Div -> S_Div
  | Diff -> S_Diff
  | Plusf -> S_Plusf
  | Minusf -> S_Minusf
  | Timesf -> S_Timesf
  | Divf -> S_Divf
  | Inf -> S_Inf
  | Infe -> S_Infe
  | Sup -> S_Sup
  | Supe -> S_Supe
  | Bor -> S_Or
  | Band -> S_And
  | Mod -> S_Mod

let rec list_of_pat p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_right (fun p acc -> list_of_pat p @ acc) t []
  | Typed (p',_s) -> list_of_pat p'


let seq_type name _inputs _outputs {pres;calls;outs} =
  let l = pres@calls@outs in
  { s_name = name;
    s_num = List.length l;
    s_attr = l
  }

let prefix_state s { pres; calls; outs} =
  { pres = List.map (fun x -> s^"_pre_"^x) pres;
    calls = List.map (fun x -> s^"_"^x^"_state") calls;
    outs = List.map (fun x -> s^"_out_"^x) outs;
  }

let nb = ref 0

(* Add pre and calls initializations  *)

let state_eq e ({pres;calls;_} as s) =
  match e.cexpression.ce_desc with
  | CApplication (i, _num, _ck, _e) ->
    incr nb;
    { s with calls = (i^string_of_int !nb)::calls }
  | CFby _ -> {s with pres = (string_of_pattern e.cpattern)::pres }
  | _ -> s

let mk_state (n:Clocking_ast.cnode) =
  let s = { pres = []; calls = []; outs = (list_of_pat n.coutputs) } in
  let l = List.fold_left
      (fun acc eq -> state_eq eq acc) s n.cequations in
  l

let update_fby eq l =
  match eq.cexpression.ce_desc with
  | CFby (_x,e) -> { cclock = eq.cclock ;
                     cpattern = prefix_pattern ~pre:"pre_" eq.cpattern ;
                     cexpression = e}::l
  | _ -> l

let update_all_fby eqs =
  List.fold_left (fun acc eq -> update_fby eq acc) [] eqs

let update_fby_zero eq l =
  match eq.cexpression.ce_desc with
  | CFby (x,_e) -> { cclock = eq.cclock ;
                     cpattern = prefix_pattern ~pre:"pre_" eq.cpattern ;
                     cexpression = x}::l
  | _ -> l

let update_all_fby_zero eqs =
  List.fold_left (fun acc eq -> update_fby_zero eq acc) [] eqs

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

(* returns all of the outputs variables of the node <name> *)
let mk_outputs e name outs =
  let rec loop outs =
    match outs with
    | [] -> []
    | x::xs -> { s_e_desc = S_Field (e,name^"_out_"^x); s_e_loc = e.s_e_loc }:: loop xs
  in
  match outs with
  | [] -> Error.print_error e.s_e_loc "a node must have an output"
  | [x] -> { s_e_desc = S_Field (e,name^"_out_"^x) ; s_e_loc = e.s_e_loc }
  | _ -> { s_e_desc = S_ETuple (loop outs) ; s_e_loc = e.s_e_loc }


(* Convert expressions  *)
let rec seq_exp e =
  let sexp = { s_e_desc = S_Unit ; s_e_loc = e.ce_loc} in
  match e.ce_desc with
  | CValue v -> { sexp with s_e_desc = S_Value v}
  | CVariable s -> { sexp with s_e_desc = S_Variable s}
  | CCall (f,e) ->
    let e' = seq_exp e in
    { sexp with s_e_desc = S_Call (f,e') }
  | CInfixOp (op,e1,e2) ->
    { sexp with s_e_desc =
                  S_InfixOp(seq_infop op,
                            seq_exp e1,
                            seq_exp e2)
    }
  | CPrefixOp (op, e) ->
    { sexp with s_e_desc = S_PrefixOp (seq_preop op, seq_exp e) }
  | CAlternative (e1,e2,e3) ->
    { sexp with s_e_desc =
                  S_Alternative (seq_exp e1,
                                 seq_exp e2,
                                 seq_exp e3)
    }
  | CUnit -> { sexp with s_e_desc = S_Unit }
  | CWhen (e',_i) ->
    { sexp with s_e_desc = (seq_exp e').s_e_desc }
  | CWhennot (e',_i) ->
    { sexp with s_e_desc = (seq_exp e').s_e_desc }
  | CETuple el ->
    let iel = List.map (fun e -> seq_exp e) el in
    { sexp with s_e_desc = S_ETuple (iel) }
  | CMerge (e1,e2,e3) ->
    { sexp with s_e_desc = S_Alternative (seq_exp e1,
                                          seq_exp e2,
                                          seq_exp e3)
    }
  | _ -> raise @@ Invalid_argument "seq_exp"



let seq_exp_list e _name =
  let sexp = { s_e_desc = S_Unit; s_e_loc = e.ce_loc } in
  let rec seq_exp_list e acc =
    match e.ce_desc with
    | CValue v -> { sexp with s_e_desc = S_Value v}::acc
    | CVariable s -> { sexp with s_e_desc = S_Variable s}::acc
    | CUnit -> { sexp with s_e_desc = S_Unit}::acc
    | CInfixOp _ ->
      let e = seq_exp e in
      e::acc
    | CETuple el ->
      begin
        match el with
        | [] -> acc
        | e::t ->
          List.fold_left (fun acc e -> seq_exp_list e acc) ((seq_exp e)::acc) t
      end
    | _ -> (seq_exp e)::acc
  in
  seq_exp_list e [] |> List.rev

(* Convert equations for the init function *)
let seq_eqs_zero eqs _sname _env =
  let rec seq_exp e =
    let sexp = { s_e_desc = S_Unit; s_e_loc = e.ce_loc} in
    match e.ce_desc with
    | CValue v -> { sexp with s_e_desc = S_Value v }
    | CFby (e,_e') -> seq_exp e
    | CWhen (e,_) -> seq_exp e
    | CWhennot (e,_) -> seq_exp e
    | CMerge (_,e1,_) -> seq_exp e1
    | _ -> { sexp with s_e_desc = S_Magic }
  in
  let split_eqs eq =
    let seq = { s_e_desc = S_Magic ; s_e_loc = eq.cexpression.ce_loc } in
    match eq.cpattern.p_desc with
    | Tuple pl ->
      List.map (fun p -> { s_pattern = p;
                           s_expression = seq })  pl

    | _ -> [{ s_pattern = eq.cpattern ; s_expression = seq} ]
  in
  let rec aux l =
    match l with
      [] -> []
    | eq::eqs ->
      begin
        match eq.cexpression.ce_desc with
        | CApplication (i,_num,_c,_e') ->
          incr nb;
          let seq = { s_pattern = eq.cpattern ;
                      s_expression = seq_exp eq.cexpression }
          in
          let seqs = split_eqs eq in
          let eq =
            { s_pattern = Parsing.mk_pattern (i^(string_of_int !nb)^"_state");
              s_expression =
                { s_e_desc = S_Application_init (i^"_alloc",!nb, [{ seq.s_expression with s_e_desc =  S_Unit}] );
                  s_e_loc = eq.cexpression.ce_loc }
            } in
          (eq::seqs)@(aux eqs)
        | _ ->
          let seq = { s_pattern = eq.cpattern ;
                      s_expression = seq_exp eq.cexpression }
          in
          seq::(aux eqs)
      end
  in
  aux eqs

(* Create init function  *)
let seq_zero name inputs outputs state env eqs =
  let sname = string_of_pattern name in
  Clocking_ast_printer.print_equations Format.std_formatter (eqs,());
  let eqs = eqs@(update_all_fby_zero eqs) in
  nb := 0;
  let eqs = seq_eqs_zero eqs sname env in
  nb := 0;
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = eqs}

let rec mk_condition (l:(bool * Clocking_ast.ident) list) =
  let sexp = { s_e_desc = S_Unit ; s_e_loc = Location.none } in
  let mk_cond (c,v) =
    if c then
      {sexp with s_e_desc = S_Variable v}
    else
      let var = {sexp with s_e_desc = S_Variable v} in
      { sexp with s_e_desc = S_PrefixOp (S_Not, var) }
  in
  match l with
  | [] ->  {sexp with s_e_desc = S_Value (Bool true) }
  | [x] ->
    mk_cond x
  | x::l' ->
    let c = mk_cond x in
    { sexp with s_e_desc = S_InfixOp (S_And, c, mk_condition l') }

(* Convert equations for the step function  *)
let seq_eqs_next eqs name env =
  let rec seq_exp s e =
    let sexp = { s_e_desc = S_Unit; s_e_loc = e.ce_loc } in
    match e.ce_desc with
    | CArray _ | CArray_get _ | CImperative_update _ | CArray_fold _ | CArray_map _ -> failwith "todo"
    | CValue v -> { sexp with s_e_desc = S_Value v}
    | CVariable s -> { sexp with s_e_desc = S_Variable s }
    | CApplication (i, _num, _c, _e) ->
      let e = (try
                 let outputs = List.assoc i !env in
                 mk_outputs
                   ({ sexp with
                      s_e_desc = S_Field
                          ({ sexp with s_e_desc = S_Variable "state"},
                           name^"_"^i^string_of_int !nb^"_state")})
                   i outputs
               with Not_found -> failwith ("unknown node "^i))
      in e
    | CCall (f,e) ->
      let se = seq_exp s e in
      { sexp with s_e_desc = S_Call (f,se) }
    | CInfixOp (op,e1,e2) ->
      { sexp with s_e_desc =
                    S_InfixOp(seq_infop op,
                              seq_exp s e1,
                              seq_exp s e2)
      }
    | CPrefixOp (op, e) ->
      { sexp with s_e_desc = S_PrefixOp (seq_preop op, seq_exp s e)}
    | CAlternative (e1,e2,e3) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s e1,
                                   seq_exp s e2,
                                   seq_exp s e3)
      }
    | CUnit -> { sexp with s_e_desc = S_Unit }
    | CArrow (_e1,e2) -> seq_exp s e2
    | CFby (_e,e') ->
      begin
        match e'.ce_desc with
        | CValue v -> { sexp with s_e_desc = S_Value v}
        | _ -> { sexp with s_e_desc =  S_Ref (name^"_pre_"^s)}
      end
    | CWhen (e',_) ->
      seq_exp s e'
    | CWhennot (e',_) ->
      seq_exp s e'
    | CETuple el ->
      let iel = List.map (fun e -> seq_exp s e) el in
      { sexp with s_e_desc =  S_ETuple (iel)}
    | CMerge (e1,e2,e3) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s e1,
                                   seq_exp s e2,
                                   seq_exp s e3)
      }
    | _ -> invalid_arg "seq"
  in
  let rec aux l  =
    match l with
      [] -> []
    | eq :: eqs' ->
      let s = string_of_pattern eq.cpattern in
      begin match eq.cexpression.ce_desc with
        (* For applications, we need to add another line to update the state ...  *)
        | CApplication(i,_num,c, e') ->
          let conds = get_condition (Ck c) in
          incr nb;
          let seq = { s_pattern = eq.cpattern;
                      s_expression = seq_exp s eq.cexpression } in

          let params = seq_exp_list e' name in
          let e = eq.cexpression in
          let state =
            { s_e_desc = S_Field(
                  {s_e_desc = S_Variable "state";
                   s_e_loc = Location.none}
                ,name^"_"^i^string_of_int !nb^"_state");
              s_e_loc = e.ce_loc } in
          let eunit = { s_e_desc = S_Unit ; s_e_loc = e.ce_loc } in
          let e = { s_e_desc = S_Application (i^"_step",!nb, (state::params)); s_e_loc = e.ce_loc } in
          let e = if conds = [] then e else { e with s_e_desc = S_Alternative(mk_condition conds,e,eunit)} in
          let eq' =
            { s_pattern = { p_desc = PUnit ; p_loc = eq.cpattern.p_loc };
              s_expression = e;
            }
          in
          eq'::seq::(aux eqs')
        | _ ->
          let seq = { s_pattern = eq.cpattern;
                      s_expression = seq_exp (string_of_pattern eq.cpattern) eq.cexpression } in
          seq::(aux eqs')
      end
  in
  aux eqs



(* Create step function  *)
let seq_next name inputs outputs state env eqs =
  let sname = string_of_pattern name in
  let eqs = eqs@(update_all_fby eqs) in
  nb := 0;
  let eqs = seq_eqs_next eqs sname env in
  nb := 0;
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = eqs }


(* Convert node  *)
let seq_node (n:Clocking_ast.cnode) env =
  let inputs = list_of_pat n.cinputs in
  let outputs = list_of_pat n.coutputs in
  let name = n.cname in
  let sname = string_of_pattern name in
  let state = mk_state n in
  nb := 0;
  env := (sname,outputs)::!env;
  { s_name = name;
    s_type = seq_type name inputs outputs (prefix_state sname state);
    s_zero = seq_zero name inputs outputs state env n.cequations;
    s_next = seq_next name inputs outputs state env n.cequations }
