
open Parsing_ast
open Parsing_ast_printer
open Clocking_ast
open Clocking_ocl

let rec print_expression fmt (ce,vars) =
  let rec print_list fmt l =
    match l with
    | [] -> ()
    | [x] -> print_expression fmt (x,vars)
    | h::t -> Format.fprintf fmt "%a,%a" print_expression (h,vars) print_list t
  in
  let rec print_update_list fmt el =
    match el with
    | [] -> ()
    | [(e,e')] -> Format.fprintf fmt "%a => %a" print_expression (e,vars) print_expression (e',vars)
    | (he,he')::te -> Format.fprintf fmt "(%a => %a),%a" print_expression (he,vars) print_expression (he',vars) print_update_list te
  in
  let print_rec fmt ce =
    let p fmt x= print_expression fmt (x,vars) in
    let ff = Format.fprintf in
    (match ce.ce_desc with
     | Clocking_ast.CAlternative (e1,e2,e3) ->
        ff fmt "if %a then %a else %a" p e1 p e2 p e3
     | Clocking_ast.CApplication (id,exp) ->
        ff fmt "%s %a" id p exp
     | Clocking_ast.CInfixOp (op,e1,e2) ->
        ff fmt "%a %a %a" p e1 print_infop op p e2
     | Clocking_ast.CPrefixOp (op,e) ->
        ff fmt "%a %a" print_preop op p e
     | Clocking_ast.CValue v ->
        ff fmt "%a" print_value v
     | Clocking_ast.CVariable v ->
        ff fmt "%s" v
     | Clocking_ast.CArray el ->
        ff fmt "[| %a |]" print_list el
     | Clocking_ast.CArray_get (a,n) ->
        ff fmt "%a.(%a)" p a p n
     | Clocking_ast.CArray_fold (a,f,acc) ->
        ff fmt "%a.fold(%a,%a)" p a Pprintast.expression f p acc
     | Clocking_ast.CArray_map (a,f) ->
        ff fmt "%a.map(%a)" p a Pprintast.expression f
     | Clocking_ast.CImperative_update (a,al) ->
        ff fmt "%a.update(%a)" p a print_update_list al
     | Clocking_ast.CFby (e1,e2) ->
        ff fmt "%a fby %a" p e1 p e2
     | Clocking_ast.CWhen (e,c) ->
        ff fmt "%a when %a" p e p c
     | Clocking_ast.CWhennot (e,c) ->
        ff fmt "%a whennot %a" p e p c
     | Clocking_ast.CETuple el ->
        ff fmt "(%a)" print_list el
     | Clocking_ast.CPre e ->
        ff fmt "(pre %a)" p e
     | Clocking_ast.CArrow (e1,e2) ->
        ff fmt "(%a -> %a)" p e1 p e2
     | Clocking_ast.CMerge (c,e1,e2) ->
        ff fmt "merge %a %a %a" p c p e1 p e2
     | Clocking_ast.CCall e ->
        ff fmt "eval %a" Pprintast.expression e
     | Clocking_ast.CUnit ->
        ff fmt "()")
  in
  match ce.ce_desc with
  | CETuple _ ->   Format.fprintf fmt "%a" print_rec ce
  | _ ->
     Format.fprintf fmt "(%a :: \027[32m%a\027[0m)" print_rec ce
  print_clock (ce.ce_clock,vars)

let print_equation fmt (eq,vars) =
  Format.fprintf fmt "\t %a = %a;\n" print_pattern eq.cpattern
  print_expression (eq.cexpression,vars)

let rec print_equations fmt (eqs,vars) =
  match eqs with
  | [] -> ()
  | [x] -> print_equation fmt (x,vars)
  | h::t -> Format.fprintf fmt "%a%a" print_equation (h,vars) print_equations (t,vars)

let print_node fmt (node,verbose) =
  let cs = node.cnode_clock in
  let Forall(gv,t) = cs in
  if verbose then
  Format.fprintf fmt "node %a %a returns:%a :: \027[32m%a\027[0m = \n%a"
    print_pattern node.cname
    print_pattern node.cinputs
    print_pattern node.coutputs
    Clocking_ocl.print_clock_scheme cs
    print_equations (node.cequations,gv)
  else
    Format.fprintf fmt "%a :: %a\n%!" print_pattern node.cname Clocking_ocl.print_clock_scheme cs
