open Clocks_checked
open Tools
open Clocking_ast
open Parsing_ast

let char_list_of_string s =
  let rec acc i l =
    if i < 0 then l
    else acc (i - 1) (s.[i] :: l) in
  acc (String.length s - 1) []

let string_of_char_list cl =
  let s = Bytes.make (List.length cl) ' ' in
  let rec loop cl i =
    match cl with
    | [] -> s
    | c::cl -> Bytes.set s i c; loop cl (i+1)
  in
  let b = loop cl 0 in
  Bytes.to_string b

let rec check_lexp_of_expression (e:expression) =
  match e.e_desc with
  | Parsing_ast.InfixOp (op,e1, e2) ->
    let e1 = check_lexp_of_expression e1 in
    let e2 = check_lexp_of_expression e2 in
    Ebinop (Plus,e1,e2) (* todo : convert the operator *)
  | Parsing_ast.Value v -> Econst (Cint 2) (* todo : convert the const *)
  | Parsing_ast.Variable v -> Evar (char_list_of_string v)
  | Parsing_ast.When (e1, e2) -> Ewhen(check_lexp_of_expression e1, char_list_of_string (ident_of_expr e2),true)
  | Parsing_ast.Whennot (e1, e2) -> Ewhen(check_lexp_of_expression e1, char_list_of_string (ident_of_expr e2),false)
  | Parsing_ast.Unit -> Econst (Cint 0) (* todo : unit *)
  | _ ->
    let s = Format.asprintf "%a : not a lexp" Parsing_ast_printer.print_expression e in
    Error.print_error e.e_loc s

let rec check_clock_of_clock (c:clock) =
  let c = Clocks.shorten c in
  match c with
  | Clocking_ast.Base -> Cbase
  (* | Clocking_ast.Var _ -> Cbase *)
  | Clocking_ast.On (c, i) -> Con (check_clock_of_clock c, char_list_of_string (Carriers.string_of_carrier i), true)
  | Clocking_ast.Onnot (c, i) -> Con (check_clock_of_clock c, char_list_of_string (Carriers.string_of_carrier i), false)
  | Clocking_ast.Arrow (c1,c2) -> check_clock_of_clock c2
  | Clocking_ast.CTuple cs -> check_clock_of_clock (List.hd cs)
  | _ ->
    let s = Format.asprintf "wrong clock : %a" Clocks.print_clock (c,[]) in
    failwith s

let rec check_clock_of_clock_scheme (c: clock_scheme) =
  match c with | Clocking_ast.Forall (_, _, c) -> check_clock_of_clock c


let rec check_cexp_of_expression (e:expression) =
  match e.e_desc with
  | Parsing_ast.Alternative (b,t,f) ->
    Eif(check_lexp_of_expression b, check_cexp_of_expression t, check_cexp_of_expression f)
  | Parsing_ast.Merge (c ,t, f) ->
    let c = ident_of_expr c in
    Emerge (char_list_of_string c, check_cexp_of_expression t, check_cexp_of_expression f)
  | _ -> Eexp (check_lexp_of_expression e)


let const_of_expression e =
  match e.e_desc with
  | Value v -> Cint 0 (* todo : convert const *)
  | _ -> failwith "not a constant"


let rec print_chk fmt c =
  match c with
  | Cbase -> Format.fprintf fmt "BASE"
  | Con (c,i,b) -> Format.fprintf fmt "(%a on (%s,%b))" print_chk c (string_of_char_list i) b

let check_equation_of_equation { cpattern; cexpression; cclock } =
  let clk = check_clock_of_clock cclock in
  (* Format.printf "Clock of %a : %a \n" Parsing_ast_printer.print_pattern cpattern print_chk clk; *)

  let s = try string_of_pattern cpattern with _ -> "tuple" in
  match cexpression.e_desc with
  | Fby (e1,e2) ->
    let le = check_lexp_of_expression e2 in
    let c = const_of_expression e1 in
    EqFby (char_list_of_string s, clk, c, le)
  | Application (i,n,e) ->
    let le = check_lexp_of_expression e in
    EqApp (char_list_of_string s,clk,char_list_of_string i, [le])
  | _ -> let ce = check_cexp_of_expression cexpression in
    EqDef (char_list_of_string s,clk,ce)

let check_equations_of_equations eqs =
  List.map (check_equation_of_equation) eqs

let check_node_of_node n =
  { n_name = char_list_of_string (string_of_pattern n.cname);
    n_input = List.map char_list_of_string (string_list_of_pattern n.cinputs);
    n_output = List.map char_list_of_string (string_list_of_pattern n.coutputs);
    n_eqs = check_equations_of_equations n.cequations
  }

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
    (char_list_of_string s, check_clock_of_clock_scheme clk)
  in
  List.map aux env

let check_node env  n =
  (* let env = List.map (fun x -> (char_list_of_string x,Cbase)) (string_list_of_pattern n.cinputs) in *)
  (* let env' = List.map (fun x -> (char_list_of_string x,Cbase)) (string_list_of_pattern n.coutputs) in *)
  (* let env = env@env' in *)
  let cn = check_node_of_node n in
  (* Format.printf "env = %a \n" Clocks.print_env env; *)
  let env = check_env_of_env env in
  Format.printf "chk env = %a \n" print_chk_env env;
  List.iter (fun eq -> Format.printf "eq : %b \n"
                (well_clocked_eq env eq)) cn.n_eqs;
  List.iter (fun eq -> Format.printf "in %s = %a \n"
                (string_of_char_list eq)
                print_chk_option (clockof_var env eq)) cn.n_input;
  List.iter (fun eq -> Format.printf "out %s = %a  \n"
                (string_of_char_list eq)
                print_chk_option (clockof_var env eq)) cn.n_output;
  well_clocked_node env cn
