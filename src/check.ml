open Clocks_checked
open Tools
open Clocking_ast
open Parsing_ast

let char_list_of_string s =
  let rec acc i l =
    if i < 0 then l
    else acc (i - 1) (s.[i] :: l) in
  acc (String.length s - 1) []

let rec nelist_of_list l =
  match l with
  | [] -> failwith "empty list"
  | [h] -> Nebase h
  | h::t -> Necons (h, nelist_of_list t)

let rec list_of_nelist nl =
  match nl with
  | Nebase c -> [c]
  | Necons (c,d) -> c::(list_of_nelist d)


let string_of_char_list cl =
  let s = Bytes.make (List.length cl) ' ' in
  let rec loop cl i =
    match cl with
    | [] -> s
    | c::cl -> Bytes.set s i c; loop cl (i+1)
  in
  let b = loop cl 0 in
  Bytes.to_string b


exception WrongClock of ck
exception WrongCt of ct
let rec check_clock_of_clock (c:ct) =
  let c = Minisimplclock.shorten_ct c in
  let rec aux c =
    match c with
    | Clocking_ast.CkBase -> Cbase
    | Clocking_ast.Ckon (c, i) ->
      Con (aux c, char_list_of_string i)
    | Clocking_ast.Ckonnot (c, i) ->
      Conot (aux c, char_list_of_string i)
    | _ -> raise (WrongClock c)
  in
  match c with
  | Ck c -> aux c
  | CkTuple cs ->
    check_clock_of_clock (List.hd cs)
 (* raise (WrongCt c) *)


let rec check_clock_of_clock_scheme (g,c: clk_scheme) =
  check_clock_of_clock c

let rec check_lexp_of_cexpression (ce:cexpression) =
  try
    match ce.ce_desc with
    | Clocking_ast.CInfixOp (op, e1, e2) ->
      let e1 = check_lexp_of_cexpression e1 in
      let e2 = check_lexp_of_cexpression e2 in
      Ebinop (Op,e1,e2)
    (* | Clocking_ast.CPrefixOp (_, _) -> (??) *) (* todo *)
    | Clocking_ast.CValue _ ->
      Econst (Cc, check_clock_of_clock ce.ce_clk)
    | Clocking_ast.CVariable v -> Evar (char_list_of_string v)
    | Clocking_ast.CWhen (e1,e2) ->
      Ewhen(check_lexp_of_cexpression e1,
            char_list_of_string (ident_of_cexpr e2))
    | Clocking_ast.CWhennot (e1,e2) ->
      Ewhenot(check_lexp_of_cexpression e1,
            char_list_of_string (ident_of_cexpr e2))
    | Clocking_ast.CUnit ->
      Econst (Cc, check_clock_of_clock ce.ce_clk)
    | _ ->
      let s = Format.asprintf "%a : not a lexp"
          Clocking_ast_printer.print_expression ce in
      Error.print_error ce.ce_loc s
  with WrongClock ck ->
    let s = Format.asprintf "%a : wrong clock"
        Minisimplclock.print_ck ck in
    Error.print_error ce.ce_loc s
     | WrongCt ct ->
       let s = Format.asprintf "%a : wrong ct"
           Minisimplclock.print_ct ct in
       Error.print_error ce.ce_loc s


let rec check_cexp_of_cexpression (ce:cexpression) =
  match ce.ce_desc with
  | Clocking_ast.CAlternative (b,t,f) ->
    Eif(check_lexp_of_cexpression b,
        check_cexp_of_cexpression t,
        check_cexp_of_cexpression f)
  | Clocking_ast.CMerge (c ,t, f) ->
    let c = ident_of_cexpr c in
    Emerge (char_list_of_string c,
            check_cexp_of_cexpression t,
            check_cexp_of_cexpression f)
  | _ -> Eexp (check_lexp_of_cexpression ce)


let const_of_cexpression ce =
  match ce.ce_desc with
  | CValue v -> Cc
  | _ -> failwith "not a constant"


let rec print_chk fmt c =
  match c with
  | Cbase -> Format.fprintf fmt "BASE"
  | Con (c,i) -> Format.fprintf fmt "(%a on %s)" print_chk c (string_of_char_list i)
  | Conot (c,i) -> Format.fprintf fmt "(%a onnot %s)" print_chk c (string_of_char_list i)

let check_equation_of_equation { cpattern; cexpression; cclock } =
  try
    let clk = check_clock_of_clock cclock in
    (* Format.printf "Clock of %a : %a \n" Parsing_ast_printer.print_pattern cpattern print_chk clk; *)
    let s = try string_of_pattern cpattern with _ -> "tuple" in
    match cexpression.ce_desc with
    | CFby (e1,e2) ->
      let le = check_lexp_of_cexpression e2 in
      let c = const_of_cexpression e1 in
      EqFby (char_list_of_string s, clk, c, le)
    | CApplication (i,n,e) ->
      let le = match e.ce_desc with CETuple e -> e | _ -> [e] in
      let le = List.map check_lexp_of_cexpression le in
      let cs = match cpattern.p_desc
        with Tuple t -> List.map string_of_pattern t
           | _ -> [s]
      in
      let cs = List.map char_list_of_string cs in
      let cs = nelist_of_list cs in
      EqApp (cs,clk,char_list_of_string i, nelist_of_list le)
    | _ -> let ce = check_cexp_of_cexpression cexpression in
      EqDef (char_list_of_string s,clk,ce)
  with WrongClock ck ->
    let s = Format.asprintf "%a : wrong clock"
        Minisimplclock.print_ck ck in
    Error.print_error cexpression.ce_loc s
  | WrongCt ct ->
       let s = Format.asprintf "%a : wrong ct"
           Minisimplclock.print_ct ct in
       Error.print_error cexpression.ce_loc s

let rec check_equations_of_equations eqs =
  match eqs with
  | [] -> failwith "empty list"
  | [e] -> Nebase (check_equation_of_equation e)
  | h::t -> Necons ((check_equation_of_equation h),(check_equations_of_equations t))

let check_node_of_node n =
  Mk_node ((char_list_of_string (string_of_pattern n.cname)),
    (List.map char_list_of_string (string_list_of_pattern n.cinputs) |> nelist_of_list),
    (List.map char_list_of_string (string_list_of_pattern n.coutputs) |> nelist_of_list),
    (check_equations_of_equations n.cequations))

let rec print_chk_option fmt c =
  match c with
    Some c -> print_chk fmt c
  | None -> Format.fprintf fmt "NONE"

let print_chk_env fmt e =
  match e with
    [] -> ()
  | (cl,clk)::t -> Format.fprintf fmt "%s :: %a \n" (string_of_char_list cl)
                     print_chk clk

let check_env_of_env env  =
  let rec aux (s,clk) =
    try
      (char_list_of_string s, check_clock_of_clock_scheme clk)
    with WrongClock ck ->
      let s = Format.asprintf "%a : wrong clock"
          Minisimplclock.print_ck ck in
      Error.print_error Location.none s
       | WrongCt ct ->
         let s = Format.asprintf "%a : wrong ct"
             Minisimplclock.print_ct ct in
         Error.print_error Location.none s

  in
  List.map aux env

let check_node env  n =
  (* let env = List.map (fun x -> (char_list_of_string x,Cbase)) (string_list_of_pattern n.cinputs) in *)
  (* let env' = List.map (fun x -> (char_list_of_string x,Cbase)) (string_list_of_pattern n.coutputs) in *)
  (* let env = env@env' in *)
  let cn = check_node_of_node n in
  (* Format.printf "env = %a \n" Clocks.print_env env; *)
  let env = check_env_of_env env in
  (* Format.printf "chk env = %a \n" print_chk_env env; *)
  (* List.iter (fun eq -> Format.printf "eq : %b \n" *)
                (* (well_clocked_eq env eq)) cn.n_eqs; *)
  (* List.iter (fun eq -> Format.printf "in %s = %a \n" *)
                (* (string_of_char_list eq) *)
                (* print_chk_option (clockof_var env eq)) (list_of_nelist cn.n_input); *)
  (* List.iter (fun eq -> Format.printf "out %s = %a  \n" *)
                (* (string_of_char_list eq) *)
                (* print_chk_option (clockof_var env eq)) (list_of_nelist cn.n_output); *)
  well_clocked_node env cn
