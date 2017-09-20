
open Seq_proof_ast
open Imperative_ast_printer
open Parsing_ast_printer
open Parsing_ast
open Proof_compiling

let rec print_expanded_list f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a " f h ; print_expanded_list f fmt t
  | _ -> ()

let rec print_expanded_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "%a" (print_expanded_list print_pattern) t
  | PUnit -> Format.fprintf fmt "()"
  | Typed (p,s) -> Format.fprintf fmt "(%a:%s)" print_pattern p s

let rec whyml_expression fmt exp =
  let whyml_preop fmt op =
    match op with
    | S_Not -> Format.fprintf fmt "not "
    | S_Neg -> Format.fprintf fmt "-"
    | S_Negf -> Format.fprintf fmt "-."
  in
  let whyml_infop fmt op =
    match op with
    | S_Diff -> Format.fprintf fmt "<>"
    | S_Equals -> Format.fprintf fmt "="
    | S_Plus -> Format.fprintf fmt "+"
    | S_Times -> Format.fprintf fmt "*"
    | S_Div -> Format.fprintf fmt "/"
    | S_Minus -> Format.fprintf fmt "-"
    | S_Minusf -> Format.fprintf fmt "-."
    | S_Divf -> Format.fprintf fmt "/."
    | S_Plusf -> Format.fprintf fmt "+."
    | S_Timesf -> Format.fprintf fmt "*."
    | S_Inf -> Format.fprintf fmt "<"
    | S_Infe -> Format.fprintf fmt "<="
    | S_Sup -> Format.fprintf fmt ">"
    | S_Supe -> Format.fprintf fmt ">="
    | S_Or -> Format.fprintf fmt "||"
    | S_And -> Format.fprintf fmt "&&"
    | S_Mod -> Format.fprintf fmt "mod"
  in
  let rec whyml_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 whyml_expression e
    | e::tl -> Format.fprintf fmt "%a,%a"
                 whyml_expression e
                 whyml_expressions tl
  in
  match exp with
  | S_Value c -> print_value fmt c
  | S_Variable v ->  Format.fprintf fmt "%s" v
  | S_Ref v -> Format.fprintf fmt "!%s" v
  | S_RefDef e -> Format.fprintf fmt "ref %a" whyml_expression e
  | S_InfixOp (op,e1,e2) -> Format.fprintf fmt "(%a %a %a)"
                             whyml_expression e1
                             whyml_infop op
                             whyml_expression e2
  | S_PrefixOp (op,e) -> Format.fprintf fmt "(%a%a)"
                          whyml_preop op
                          whyml_expression e
  | S_Alternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                 whyml_expression e1
                                 whyml_expression e2
                                 whyml_expression e3
  | S_Unit -> Format.fprintf fmt "()"
  | S_Application (i,num,e) -> Format.fprintf fmt "%s%d_step (%a)"
                              i
                              num
                             whyml_expression e
  | S_Application_init (i,e) ->
     Format.fprintf fmt "%s ()" i
  | S_Call e -> Format.fprintf fmt "(_____)"
  | S_Constr s -> Format.fprintf fmt "%s" s
  | S_ExpTuple el -> Format.fprintf fmt "(%a)"
                    whyml_expressions el

let whyml_equations fmt el =
  let whyml_equation fmt e =
    Format.fprintf fmt "let %a = %a in\n"
      print_pattern e.s_pattern
      whyml_expression e.s_expression
  in
  List.iter (fun x -> whyml_equation fmt x) el


let rec prefix_expression exp prefix =
 match exp with
  | S_Value c -> exp
  | S_Variable v -> S_Variable (prefix^v)
  | S_Ref v -> exp
  | S_RefDef e -> exp
  | S_InfixOp (op,e1,e2) -> S_InfixOp (op,
                             prefix_expression e1 prefix,
                             prefix_expression e2 prefix)
  | S_PrefixOp (op,e) -> S_PrefixOp (op,
                          prefix_expression e prefix)
  | S_Alternative (e1,e2,e3) -> S_Alternative (
                                 prefix_expression e1 prefix,
                                 prefix_expression e2 prefix,
                                 prefix_expression e3 prefix)
  | S_Unit -> exp
  | S_Application (i,num,e) -> S_Application (
                              i,
                              num,
                             prefix_expression e prefix)
  | S_Application_init (i,e) ->
     S_Application_init (i, prefix_expression e prefix)
  | S_Call e -> exp
  | S_Constr s -> exp
  | S_ExpTuple el -> S_ExpTuple (List.map (fun e -> prefix_expression e prefix) el)

let prefix_equation eq prefix =
  { s_pattern = eq.s_pattern ; s_expression = prefix_expression eq.s_expression prefix }

let rec prefix_pattern p prefix =
  match p.p_desc with
  | Ident i -> {p_desc =  Ident (prefix^i) ; p_loc = Location.none}
  | Tuple pl -> {p_desc = Tuple (List.map (fun px -> prefix_pattern px prefix ) pl) ; p_loc = Location.none }
  | PUnit -> { p_desc = PUnit ; p_loc = Location.none }
  | Typed (p,s) ->{ p_desc =  Typed (prefix_pattern p prefix, s) ;
                    p_loc = Location.none }


let print_pre fmt (p,ins) =
  match p with
  | None -> ()
  | Some x ->
    Format.fprintf fmt "requires {  %a }"
      whyml_expression x


let print_pre_inv fmt (p,inv,st,ins) =
  match p,inv with
  | Some x , None  ->
    Format.fprintf fmt "requires {  %a }"

      whyml_expression x
  | Some x , Some y ->
    Format.fprintf fmt "requires { %a && %a } \n"

      whyml_expression x
      whyml_expression (prefix_expression y "pre_")
  | None, Some y ->
    Format.fprintf fmt "requires { %a } \n"


      whyml_expression (prefix_expression y "pre_")
  | _ -> ()


let print_assume fmt i =
  match i with
  | None -> ()
  | Some x ->
     Format.fprintf fmt "assume { %a };"
      whyml_expression (prefix_expression x "pre_")

let print_assert fmt (i,st) =
  match i with
  | None -> ()
  | Some x ->
     Format.fprintf fmt "assert { let %a = s in %a };"
      print_pattern st
      whyml_expression (prefix_expression x "post_")

let print_post fmt (p,outs) =
  match p with
  | None -> ()
  | Some x ->
    Format.fprintf fmt "ensures { let %a = result in %a } \n"
      print_pattern outs
      whyml_expression x

let print_post_inv fmt (p,i,st,outs) =
  match p,i with
  | Some x, None ->
    Format.fprintf fmt "ensures { let (%a,%a) = result in %a } \n"
      print_pattern st
      print_pattern outs
      whyml_expression x
  | None, Some y ->
    Format.fprintf fmt "ensures { let (%a,%a) = result in %a } \n"
      print_pattern st
      print_pattern outs
      whyml_expression (prefix_expression y "post_")
  | Some x, Some y ->
    Format.fprintf fmt "ensures { let (%a,%a) = result in %a && %a } \n"
      print_pattern st
      print_pattern outs
      whyml_expression x
      whyml_expression (prefix_expression y "post_")
  | _ -> ()

let whyml_app_inits fmt l =
  whyml_equations fmt l

let get_patterns el =
  List.map (fun e -> e.s_pattern) el

(*let concat_pattern p1 p2 =
  match p1.p_desc,p2.p_desc with
  | PUnit, _ -> p2
  | _ , PUnit -> p1
  | _,_ -> { p_desc = Tuple [p1;p2] ; p_loc = Location.none}
*)


let whyml_fun_init fmt s_init =
  let name = s_init.si_name in
  let pats = get_patterns s_init.si_equations in
  let inputs = s_init.si_inputs in
  let outputs = s_init.si_outputs in
  let post = s_init.si_post in
  let pre = s_init.si_pre in
  let inv = s_init.si_inv in
  let tpats =
    match pats with
    | [x] -> x
    | _ -> { p_desc = Tuple pats ; p_loc = Location.none }  in
  let tpats = concat_pat tpats inputs in
    let tpats = flatten_pat tpats in
  let tpats_post = prefix_pattern tpats "post_" in
  Format.fprintf fmt
    "let %a_init %a =
@[%a
%a%a(%a,%a)@]"
    print_pattern name
    print_expanded_pattern inputs
  print_pre (pre,inputs)
  print_post_inv (post,inv, tpats_post,outputs)
  whyml_equations s_init.si_equations
  print_pattern tpats
  print_pattern outputs

let whyml_fun_step fmt s_step =
  let name = s_step.ss_name in
  let inputs = s_step.ss_inputs in
  let outputs = s_step.ss_outputs in
  let post = s_step.ss_post in
  let pre = s_step.ss_pre in
  let inv = s_step.ss_inv in
  let pats = get_patterns s_step.ss_equations in
  let tpats =
    match pats with
    | [x] -> x
    | _ -> { p_desc = Tuple pats ; p_loc = Location.none }  in
  let tpats = flatten_pat tpats in
  let tpats = concat_pat tpats inputs in
  let tpats = flatten_pat tpats in
  let tpats_pre = prefix_pattern tpats "pre_" in
  let tpats_post = prefix_pattern tpats "post_" in
  Format.fprintf fmt
    "let %a_step %a %a =\n@[%a\n%a\n%a\n(%a,%a)@]"
    print_pattern name
    print_expanded_pattern tpats_pre
    print_expanded_pattern inputs
  print_pre_inv (pre,inv, tpats_pre,inputs)
  print_post_inv (post,inv,tpats_post,outputs)
  whyml_equations s_step.ss_equations
  print_pattern tpats
  print_pattern outputs

let whyml_node fmt node =
  let pats = get_patterns node.s_init_fun.si_equations in
   let tpats =
    match pats with
    | [x] -> x
    | _ -> { p_desc = Tuple pats ; p_loc = Location.none }  in
  let tpats = concat_pat tpats node.s_inputs in
  let tpats_pre = prefix_pattern tpats "pre_" in
  let tpats_post = prefix_pattern tpats "post_" in
  Format.fprintf fmt "
@[let %a () =
(* apps inits *)
@[%a@]
(* instant 0 *)\n@[%a@]
in
(* instant n *)\n@[%a@]
in
@[(* switch between instant 0 and instant n >=0 *)
 let state = ref None in
  fun %a %a ->
   @[%a
   match !state with
   | None -> let (s, result) = (%a_init %a ) in
             %a
             (state := Some s; result)
   | Some s' ->
             let %a = s' in
             %a
             let (s, result) = (%a_step %a %a ) in
             %a
             (state := Some s; result)
  end@]
@]@]"
    print_pattern node.s_name

    whyml_app_inits node.s_apps_init
    whyml_fun_init node.s_init_fun
    whyml_fun_step node.s_step_fun
        print_expanded_pattern node.s_inputs
    print_pre (node.s_pre,node.s_inputs)
    print_post (node.s_post,node.s_outputs)

    print_pattern node.s_name
    print_expanded_pattern node.s_inputs
    print_assert (node.s_inv,tpats_post)
    print_pattern tpats_pre
    print_assume (node.s_inv)
    print_pattern node.s_name
    print_expanded_pattern tpats_pre
    print_expanded_pattern node.s_inputs
    print_assert (node.s_inv,tpats_post)
