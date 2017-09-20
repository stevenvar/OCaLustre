open Parsing_ast
open Sequential_ast
open Error

let nb = ref 0


(* Functions for exploding tuples : (a,b) = 1,2 becomes a=1;b=2  *)

let explode_pattern p =
  match p.p_desc with
  | Tuple pl -> pl
  | _ -> [p]

let explode_exp e =
  match e.s_e_desc with
  | S_ETuple el -> el
  | _ -> [e]

let rec zip_tuple pl el loc =
  match pl,el with
  | [],[] -> []
  | [], _ | _, [] ->
    Error.print_error loc "In tuple : not the same number in element in pattern and expression"
  | x::xs , y::ys -> {s_pattern = x; s_expression =y}::(zip_tuple xs ys loc)

let explode_equation eq =
  let pl = explode_pattern eq.s_pattern in
  let el = explode_exp eq.s_expression in
  zip_tuple pl el eq.s_pattern.p_loc

let explode_equations leq =
  List.fold_right (fun eq acc -> (explode_equation eq)@acc) leq []


(* ---  *)

let prefix_pattern pre p =
  match p.p_desc with
  | Ident i -> { p with p_desc = Ident (pre^i) }
  | _ -> Error.print_error p.p_loc "This is not a variable"

let rec string_of_pattern p =
  match p.p_desc with
  | Ident i -> i
  | _ -> "?"
  (* | _ -> assert false *)

let rec update_fby eq l =
  match eq.expression.e_desc with
  | Fby (x,e) -> { pattern = prefix_pattern "pre_" eq.pattern ; expression = e}::l
  | _ -> l

let update_all_fby eqs =
  List.fold_left (fun acc eq -> update_fby eq acc) [] eqs


let rec list_of_pat p =
  match p.p_desc with
  | PUnit -> []
  | Ident x -> [x]
  | Tuple t -> List.fold_right (fun p acc -> list_of_pat p @ acc) t []
  | Typed (p',s) -> list_of_pat p'

(* returns all of the outputs variables of the node <name> *)
let rec mk_outputs e name outs =
  let rec loop outs =
    match outs with
    | [] -> []
    | x::xs -> { s_e_desc = S_Field (e,name^"_out_"^x); s_e_loc = e.s_e_loc }:: loop xs
  in
  match outs with
  | [] -> Error.print_error e.s_e_loc "a node must have an output"
  | [x] -> { s_e_desc = S_Field (e,name^"_out_"^x) ; s_e_loc = e.s_e_loc }
  | _ -> { s_e_desc = S_ETuple (loop outs) ; s_e_loc = e.s_e_loc }

(* Convert constructors  *)

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

(* Convert expressions  *)

let rec seq_exp e =
  let sexp = { s_e_desc = S_Unit ; s_e_loc = e.e_loc} in
  match e.e_desc with
  | Value v -> { sexp with s_e_desc = S_Value v}
  | Variable s -> { sexp with s_e_desc = S_Variable s}
  | Call e ->
    { sexp with s_e_desc = S_Call e }
  | InfixOp (op,e1,e2) ->
    { sexp with s_e_desc =
                  S_InfixOp(seq_infop op,
                            seq_exp e1,
                            seq_exp e2)
    }
  | PrefixOp (op, e) ->
    { sexp with s_e_desc = S_PrefixOp (seq_preop op, seq_exp e) }
  | Alternative (e1,e2,e3) ->
    { sexp with s_e_desc =
                  S_Alternative (seq_exp e1,
                                 seq_exp e2,
                                 seq_exp e3)
    }
  | Unit -> { sexp with s_e_desc = S_Unit }
  | When (e',i) ->
    { sexp with s_e_desc =
                  S_Alternative (seq_exp i,
                                 seq_exp e',
                                 { s_e_desc = S_Value Nil;
                                   s_e_loc = Location.none }
                                )
    }
  | Whennot (e',i) ->
    { sexp with s_e_desc =
                  S_Alternative (seq_exp i,
                                 {s_e_desc = S_Value Nil;
                                  s_e_loc = Location.none },
                                 seq_exp e')
    }
  | ETuple el ->
    let iel = List.map (fun e -> seq_exp e) el in
    { sexp with s_e_desc = S_ETuple (iel) }
  | Merge (e1,e2,e3) ->
    { sexp with s_e_desc = S_Alternative (seq_exp e1,
                                          seq_exp e2,
                                          seq_exp e3)
    }
  | _ -> raise @@ Invalid_argument "seq_exp"



let seq_exp_list e name =
  let sexp = { s_e_desc = S_Unit; s_e_loc = e.e_loc } in
  let rec seq_exp_list e acc =
    match e.e_desc with
    | Value v -> { sexp with s_e_desc = S_Value v}::acc
    | Variable s -> { sexp with s_e_desc = S_Variable s}::acc
    | Unit -> { sexp with s_e_desc = S_Unit}::acc
    | ETuple el ->
      begin
        match el with
        | [] -> acc
        | e::t ->
          List.fold_left (fun acc e -> seq_exp_list e acc) ((seq_exp e)::acc) t
      end
    | _ ->
      Parsing_ast_printer.print_expression Format.std_formatter e;
      raise @@ Invalid_argument "seq_exp_list"
  in
  seq_exp_list e [] |> List.rev

(* Convert equations for the init function *)

let seq_eqs_zero eqs sname env =
  let rec seq_exp e =
    let sexp = { s_e_desc = S_Unit ; s_e_loc = e.e_loc} in
    match e.e_desc with
    | Value v -> { sexp with s_e_desc = S_Value v }
    | Variable s -> { sexp with s_e_desc = S_Variable s }
    | Application (i, e) ->
      (try
         let outputs = List.assoc i !env in
         mk_outputs
           ({ sexp with s_e_desc = S_Variable (i^string_of_int !nb^"_state")})
           i
           outputs
       with Not_found -> Error.print_error e.e_loc ("unknown node "^i))
    | Call e ->
      { sexp with s_e_desc = S_Call e }
    | InfixOp (op,e1,e2) ->
      { sexp with s_e_desc =
                    S_InfixOp(seq_infop op,
                              seq_exp e1,
                              seq_exp e2)
      }
    | PrefixOp (op, e) ->
      { sexp with s_e_desc = S_PrefixOp (seq_preop op, seq_exp e) }
    | Alternative (e1,e2,e3) ->
      { sexp with s_e_desc = S_Alternative (seq_exp e1,
                                            seq_exp e2,
                                            seq_exp e3) }
    | Unit -> { sexp with s_e_desc = S_Unit}
    | Arrow (e1,e2) -> seq_exp e1
    | Pre e -> assert false
    | Fby (e,e') -> seq_exp e
    | When (e',i) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp i,
                                   seq_exp e',
                                   { s_e_desc = S_Value Nil;
                                     s_e_loc = Location.none } )
      }
    | Whennot (e',i) ->
      { sexp with s_e_desc = S_Alternative (seq_exp i,
                                            { s_e_desc = S_Value Nil;
                                              s_e_loc = Location.none },
                                            seq_exp e')
      }
    | ETuple el ->
      let iel = List.map (fun e -> seq_exp e) el in
      { sexp with s_e_desc = S_ETuple (iel) }
    | Merge (e1,e2,e3) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp e1,
                                   seq_exp e2,
                                   seq_exp e3)
      }
  in
  let rec aux l =
    match l with
      [] -> []
    | eq::eqs ->
      begin
        match eq.expression.e_desc with
        | Application (i,e') ->
          incr nb;
          let seq = { s_pattern = eq.pattern ;
                  s_expression = seq_exp eq.expression }
          in
          (* let s = !nb in *)
          let eq =
            { s_pattern = Parsing_ocl.mk_pattern (i^(string_of_int !nb)^"_state");
              s_expression =
                { s_e_desc = S_Application_init (i^"_init",!nb, seq_exp_list e' sname);
                  s_e_loc = eq.expression.e_loc }
            } in
          eq::seq::(aux eqs)
        | _ ->
          let seq = { s_pattern = eq.pattern ;
                      s_expression = seq_exp eq.expression }
          in
          seq::(aux eqs)
      end
  in
  aux eqs


(* Convert equations for the step function  *)

let rec seq_eqs_next eqs name env =
  let rec seq_exp s e =
    let sexp = { s_e_desc = S_Unit; s_e_loc = e.e_loc } in
    match e.e_desc with
    | Value v -> { sexp with s_e_desc = S_Value v}
    | Variable s -> { sexp with s_e_desc = S_Variable s }
    | Application (i, e) ->
      (try
         let outputs = List.assoc i !env in
         mk_outputs
           ({ sexp with
              s_e_desc = S_Field
                  ({ sexp with s_e_desc = S_Variable "state"},
                   name^"_"^i^string_of_int !nb^"_state")})
           i outputs
       with Not_found -> failwith ("unknown node "^i))
    | Call e ->
      { sexp with s_e_desc = S_Call e }
    | InfixOp (op,e1,e2) ->
      { sexp with s_e_desc =
                    S_InfixOp(seq_infop op,
                              seq_exp s e1,
                              seq_exp s e2)
      }
    | PrefixOp (op, e) ->
      { sexp with s_e_desc = S_PrefixOp (seq_preop op, seq_exp s e)}
    | Alternative (e1,e2,e3) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s e1,
                                   seq_exp s e2,
                                   seq_exp s e3)
      }
    | Unit -> { sexp with s_e_desc = S_Unit }
    | Arrow (e1,e2) -> seq_exp s e2
    | Pre e' ->
      begin
        match e'.e_desc with
        | Value v -> { sexp with s_e_desc = S_Value v }
        | Variable n -> { sexp with s_e_desc = S_Ref ("_pre_"^n)}
        | _ -> assert false
      end
    | Fby (e,e') ->
      begin
        match e'.e_desc with
        | Value v -> { sexp with s_e_desc = S_Value v}
        | _ -> { sexp with s_e_desc =  S_Ref (name^"_pre_"^s)}
      end
    | When (e',i) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s i,
                                   seq_exp s e',
                                   {s_e_desc = S_Value Nil;
                                    s_e_loc = Location.none})
      }
    | Whennot (e',i) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s i,
                                   {s_e_desc = S_Value Nil;
                                    s_e_loc = Location.none},
                                   seq_exp s e')
      }
    | ETuple el ->
      let iel = List.map (fun e -> seq_exp s e) el in
      { sexp with s_e_desc =  S_ETuple (iel)}
    | Merge (e1,e2,e3) ->
      { sexp with s_e_desc =
                    S_Alternative (seq_exp s e1,
                                   seq_exp s e2,
                                   seq_exp s e3)
      }
  in
  let rec aux l  =
    match l with
      [] -> []
    | eq :: eqs' ->
      let s = string_of_pattern eq.pattern in
      begin match eq.expression.e_desc with
        (* For applications, we need to add another line to update the state ...  *)
        | Application(i,e') ->
          incr nb;
          let seq = { s_pattern = eq.pattern;
                      s_expression = seq_exp s eq.expression } in

          let params = seq_exp_list e' name in
          let e = eq.expression in
          let state =
            { s_e_desc = S_Field(
                  {s_e_desc = S_Variable "state";
                   s_e_loc = Location.none}
                ,name^"_"^i^string_of_int !nb^"_state");
              s_e_loc = e.e_loc } in
          let eq' =
          { s_pattern = Parsing_ocl.mk_pattern ("_"^i^(string_of_int !nb)^"_state");
            s_expression = { s_e_desc = S_Application (i^"_step",!nb, (state::params));
                             s_e_loc = e.e_loc }
          } in
          eq'::seq::(aux eqs')
        | _ ->
          let seq = { s_pattern = eq.pattern;
                      s_expression = seq_exp (string_of_pattern eq.pattern) eq.expression } in
                    seq::(aux eqs')
      end
  in
  aux eqs


(* Add pre and calls initializations  *)

let state_eq e ({pres;calls;outs} as s) =
  match e.expression.e_desc with
  | Application (i, e) ->
    incr nb;
    { s with calls = (i^string_of_int !nb)::calls }
  | Pre e' ->
    begin
      match e'.e_desc with
      (* | Value v -> s *)
      | Variable n -> { s with pres = n::pres }
      | _ -> assert false
    end
  | Fby (_,e') ->
    begin
      match e'.e_desc with
      (* | Value v -> s *)
      | _ -> {s with pres = (string_of_pattern e.pattern)::pres }
      (* assert false *)
    end
  | _ -> s

(* --  *)

module IdentSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = ident
  end )

(* A state is a record with previous values of eqs,
 ** states of called nodes and outputs  *)
let mk_state n =
  let s = { pres = []; calls = []; outs = (list_of_pat n.outputs) } in
  let l = List.fold_left
      (fun acc eq -> state_eq eq acc) s n.equations in
  l

let seq_type name inputs outputs {pres;calls;outs} =
  let l = pres@calls@outs in
  { s_name = name;
    s_num = List.length l;
    s_attr = l
  }

(* Create init function  *)

let seq_zero name inputs outputs state env eqs =
  let sname = string_of_pattern name in
  (* let call_init = *)
  (*   List.fold_left *)
  (*     (fun acc eq -> call_state_zero eq.expression acc sname) [] eqs in *)
  (* nb := 0; *)
  let eqs = eqs@(update_all_fby eqs) in
  let eqs = seq_eqs_zero eqs sname env in

  let eqs = explode_equations eqs in
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = eqs}

(* Create step function  *)

let seq_next name inputs outputs state env eqs =
  let sname = string_of_pattern name in
  (* let call_init = *)
  (*   List.fold_left *)
  (*     (fun acc eq -> call_state_next eq.expression acc sname) [] eqs in *)
  (* nb := 0; *)
  let eqs = eqs@(update_all_fby eqs) in
  let eqs = seq_eqs_next eqs sname env in
  let eqs = explode_equations eqs in
  nb := 0;
  { s_name = name;
    s_inputs = inputs;
    s_outputs = outputs;
    s_state = state;
    s_eqs = eqs }

let prefix_state s { pres; calls; outs} =
  { pres = List.map (fun x -> s^"_pre_"^x) pres;
    calls = List.map (fun x -> s^"_"^x^"_state") calls;
    outs = List.map (fun x -> s^"_out_"^x) outs;
  }

(* Convert node  *)
let seq_node n env =
  let inputs = list_of_pat n.inputs in
  let outputs = list_of_pat n.outputs in
  let name = n.name in
  let sname = string_of_pattern name in
  let state = mk_state n in
  nb := 0;
  env := (sname,outputs)::!env;
  { s_name = name;
    s_type = seq_type name inputs outputs (prefix_state sname state);
    s_zero = seq_zero name inputs outputs state env n.equations;
    s_next = seq_next name inputs outputs state env n.equations }
