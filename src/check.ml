open Clocks_checked
open Tools
open Clocking_ast
open Parsing_ast

let char_list_of_string s =
  let rec acc i l =
    if i < 0 then l
    else acc (i - 1) (s.[i] :: l) in
  acc (String.length s - 1) []

let check_identifier_of_cpattern p =
  match p.p_desc with
  | Ident x -> char_list_of_string x
  | _ -> invalid_arg "check_id_of_pattern"

let rec pattern_of_list l =
  match l with
  | [] -> Patp_nil
  | [x] -> Patp_var (check_identifier_of_cpattern x)
  | x::xs -> Patp_tuple (check_identifier_of_cpattern x, pattern_of_list xs)

let rec list_of_pattern p =
  match p with
  | Patp_nil -> []
  | Patp_var x -> [x]
  | Patp_tuple (x, p) -> x::(list_of_pattern p)

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
let check_clock_of_clock (c:ct) =
  let c = Clocking.shorten_ct c in
  let rec aux c =
    match c with
    | Clocking_ast.CkBase -> Ckbase
    | Clocking_ast.Ckon (c, i) ->
      Ckon (aux c, char_list_of_string i)
    | Clocking_ast.Ckonnot (c, i) ->
      Ckonnot (aux c, char_list_of_string i)
    | _ -> Ckbase
  in
  match c with
  | Ck c -> aux c
  | CkTuple _cs -> raise (WrongCt c)

(* raise (WrongCt c) *)

let rec cktuple_of_list l =
  match l with
  | [] -> invalid_arg "cktuple_of_list"
  | [ck] -> check_clock_of_clock ck
  | ck::cks -> Cktuple (check_clock_of_clock ck, cktuple_of_list cks)

let check_clocks_of_clocks (c:ct) =
  match c with
  | Ck _ -> (check_clock_of_clock c)
  | CkTuple (cks) -> cktuple_of_list cks


let check_clock_of_clock_scheme (_g,c: clk_scheme) =
  check_clock_of_clock c

let check_binop_of_binop op =
  match op with
  | _ -> Binopop_int (* we don't care which binop it is as the binop doesnt interfere with the typing rule *)

let check_constant_of_constant k =
  match k with
  | Integer _ -> Kint
  | Float _ -> Kfloat
  | Bool _ -> Kbool
  | Enum _ -> Kint (* little hack : constructors can be seen as ints : it does not change the clocking semantics (it's useful because the formal specification does not mention that a fby can have an enum on the left side...)  *)
  | _ -> invalid_arg "check_constant_of_constant"

let rec check_lexp_of_cexpression (ce:cexpression) =
  try
    match ce.ce_desc with
    | Clocking_ast.CInfixOp (op, e1, e2) ->
      let e1 = check_lexp_of_cexpression e1 in
      let e2 = check_lexp_of_cexpression e2 in
      Ebinop (e1,check_binop_of_binop op,e2)
    | Clocking_ast.CPrefixOp (_, e) ->
      Eunop (Unopnot, check_lexp_of_cexpression e)
    | Clocking_ast.CValue (Enum s) ->
      Econstructor (char_list_of_string s, check_clock_of_clock ce.ce_clk)
    | Clocking_ast.CValue k ->
      Econst (check_constant_of_constant k, check_clock_of_clock ce.ce_clk)
    | Clocking_ast.CVariable v -> Evar (char_list_of_string v)
    | Clocking_ast.CWhen (e1,e2) ->
      Ewhen(check_lexp_of_cexpression e1,
            char_list_of_string (ident_of_cexpr e2))
    | Clocking_ast.CWhennot (e1,e2) ->
      Ewhennot(check_lexp_of_cexpression e1,
               char_list_of_string (ident_of_cexpr e2))
    | Clocking_ast.CUnit ->
      Eunit (check_clock_of_clock ce.ce_clk)
    | _ ->
      let s = Format.asprintf "%a : unsupported expression"
          Clocking_ast_printer.print_expression ce in
      Error.print_error ce.ce_loc s
  with WrongClock ck ->
    let s = Format.asprintf "%a : wrong clock"
        Clocking_ast_printer.print_ck ck in
    Error.print_error ce.ce_loc s
     | WrongCt ct ->
       let s = Format.asprintf "%a : wrong ct"
           Clocking_ast_printer.print_ct ct in
       Error.print_error ce.ce_loc s

let rec check_lexps_of_cexpressions (ces:cexpression list) =
  match ces with
  | [] -> invalid_arg "check_lexps_of_cexpressions"
  | [e] -> Esone_exp  (check_lexp_of_cexpression e)
  | e::es -> Escons_exps (check_lexp_of_cexpression e, check_lexps_of_cexpressions es)

let rec check_cexp_of_cexpression (ce:cexpression) =
  match ce.ce_desc with
  | Clocking_ast.CAlternative (b,t,f) ->
    Ceif(check_lexp_of_cexpression b,
         check_cexp_of_cexpression t,
         check_cexp_of_cexpression f)
  | Clocking_ast.CMerge (c ,t, f) ->
    let c = ident_of_cexpr c in
    Cemerge (char_list_of_string c,
             check_cexp_of_cexpression t,
             check_cexp_of_cexpression f)
  | _ -> Ceexp (check_lexp_of_cexpression ce)


let const_of_cexpression ce =
  match ce.ce_desc with
  | CValue k -> check_constant_of_constant k
  | _ -> failwith "not a constant"


let rec print_chk fmt c =
  let rec print_tuple fmt l =
    match l with
    | Cktuple (x,xs) -> Format.fprintf fmt "%a,%a" print_chk x print_tuple xs
    | _ -> print_chk fmt c
  in
  match c with
  | Ckbase -> Format.fprintf fmt "BASE"
  | Ckon (c,i) -> Format.fprintf fmt "(%a on %s)" print_chk c (string_of_char_list i)
  | Ckonnot (c,i) -> Format.fprintf fmt "(%a onnot %s)" print_chk c (string_of_char_list i)
  | Cktuple (c,_ck) -> Format.fprintf fmt "(%a)" print_tuple c
  | Ckarrow (c,c') -> Format.fprintf fmt "(%a -> %a)" print_chk c print_chk c'


let rec check_pattern_of_pattern (cpattern: Parsing_ast.pattern) =
  match cpattern.p_desc with
  | Ident x -> Patp_var (char_list_of_string x)
  | Typed (p,_s) -> check_pattern_of_pattern p
  | Tuple pl -> pattern_of_list  pl
  | PUnit -> Patp_nil


let rec check_var_of_pattern (cpattern: Parsing_ast.pattern) =
  match cpattern.p_desc with
  | Ident x ->  (char_list_of_string x)
  | Typed (p,_s) -> check_var_of_pattern p
  | PUnit -> failwith "not unit"
  | _ -> failwith "No tuples"


let rec nb_lexps es =
  match es with
  | Esone_exp _ -> 1
  | Escons_exps (_,es) -> 1 + nb_lexps es



let check_equation_of_equation { cpattern; cexpression; cclock } =
  try
    match cexpression.ce_desc with
    | CFby (e1,e2) ->
      let clk = check_clock_of_clock cclock in
      let le = check_lexp_of_cexpression e2 in
      let c = const_of_cexpression e1 in
      EqFby (check_var_of_pattern cpattern, clk, c, le)
    | CApplication (i,_n,c,e) ->
      let le = match e.ce_desc with
        | CETuple es -> check_lexps_of_cexpressions es
        | _ -> check_lexps_of_cexpressions [e]
      in
      let cp = check_pattern_of_pattern cpattern in
      let cc = check_clock_of_clock (Ck c) in
      let cl = char_list_of_string i in
      EqApp (cp,cc,cl,le)
    | CCall (f,e) ->
      EqEval (check_var_of_pattern cpattern,
              check_clock_of_clock cclock ,
              char_list_of_string f,
              check_lexp_of_cexpression e)
    | _ ->
      let clk = check_clock_of_clock cclock in
      let ce = check_cexp_of_cexpression cexpression in
      EqDef (check_var_of_pattern cpattern,clk,ce)
  with WrongClock ck ->
    let s = Format.asprintf "%a : wrong clock"
        Clocking_ast_printer.print_ck ck in
    Error.print_error cexpression.ce_loc s
     | WrongCt ct ->
       let s = Format.asprintf "%a : wrong ct"
           Clocking_ast_printer.print_ct ct in
       Error.print_error cexpression.ce_loc s

let rec check_equations_of_equations eqs =
  match eqs with
  | [] -> failwith "empty list"
  | [e] -> Eqseqs_one (check_equation_of_equation e)
  | h::t -> Eqseqs_cons (check_equation_of_equation h,
                         check_equations_of_equations t)

let check_node_of_node n =
  Nodemk_node (
    (char_list_of_string (string_of_pattern n.cname)),
    check_pattern_of_pattern n.cinputs,
    check_pattern_of_pattern n.coutputs,
    check_equations_of_equations n.cequations
  )

let check_env_of_env env  =
  let aux (s,clk) =
    try
      (char_list_of_string s, check_clock_of_clock_scheme clk)
    with WrongClock ck ->
      let s = Format.asprintf "%a : wrong clock"
          Clocking_ast_printer.print_ck ck in
      Error.print_error Location.none s
       | WrongCt ct ->
         let s = Format.asprintf "%a : wrong ct"
             Clocking_ast_printer.print_ct ct in
         Error.print_error Location.none s

  in
  List.map aux env

let check_global_env_of_global_env global =
  let aux (s,(clk1,xs1,clk2,xs2)) =
    try
      let name = char_list_of_string s in
      let inc = check_clocks_of_clocks clk1 in
      let outc = check_clocks_of_clocks clk2 in
      let inp = check_pattern_of_pattern xs1 in
      let outp = check_pattern_of_pattern xs2 in
      (name, (Signcons (inp,inc,outp,outc)))
    with WrongClock ck ->
      let s = Format.asprintf "%a : wrong clock"
          Clocking_ast_printer.print_ck ck in
      Error.print_error Location.none s
       | WrongCt ct ->
         let s = Format.asprintf "%a : wrong ct"
             Clocking_ast_printer.print_ct ct in
         Error.print_error Location.none s

  in
  List.map aux global

let rec print_chk_env fmt l =
  match l with
  | [] -> ()
  | (n,c)::xs -> Format.fprintf fmt "%s : %a \n%a" (string_of_char_list n)
                   print_chk c print_chk_env xs


let check_node global local n =
  let cn = check_node_of_node n in
  let global = check_global_env_of_global_env global in
  let local = check_env_of_env local in
  let b = well_clocked_prog [(local,cn)] global in
  if not b then print_chk_env Format.std_formatter local;
  b
