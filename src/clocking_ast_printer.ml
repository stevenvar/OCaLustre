
open Parsing_ast
open Parsing_ast_printer
open Clocking_ast


let rec print_expression fmt (ce:cexpression) =
  let rec print_expression_list fmt el =
    match el with
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" print_expression e
    | he::te -> Format.fprintf fmt "%a,%a"
                  print_expression he
                  print_expression_list te
  in
  match ce.ce_desc with
  | CVariable i -> Format.fprintf fmt "%a"
                    print_ident i
  | CArray e -> Format.fprintf fmt "[| %a |]" print_expression_list e
  | CArray_get (e,e') -> Format.fprintf fmt "%a.(%a)"
                           print_expression e
                           print_expression e'
  | CArray_fold (e,f,acc) -> Format.fprintf fmt "%a.fold(%a,%a)"
                               print_expression e Pprintast.expression f
                               print_expression acc
  | CArray_map (e,f) ->Format.fprintf fmt "%a.map(%a)"
                         print_expression e
                         Pprintast.expression f
  | CImperative_update (e,el) -> Format.fprintf fmt "%a where (...)"
                                   print_expression e
  | CAlternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))"
      print_expression e1
      print_expression e2
      print_expression e3
  | CApplication (i,_,e) ->
     Format.fprintf fmt "(%a %a)"
                    print_ident i
                    print_expression e
  | CCall (e) ->
     Format.fprintf fmt "(eval (%s))" (Pprintast.string_of_expression e)
  | CInfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      print_expression e1
      print_infop op
      print_expression e2
  | CPre e -> Format.fprintf fmt "(pre %a)" print_expression e
  | CPrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)"
                           print_preop op
                           print_expression e1

  | CValue v -> print_value fmt v
  | CArrow (e1,e2) -> Format.fprintf fmt "(%a -> %a)"
                       print_expression e1
                       print_expression e2
  | CFby (e1, e2) -> Format.fprintf fmt "(%a fby %a)"
                    print_expression e1
                    print_expression e2
  | CUnit -> Format.fprintf fmt "()"
  | CWhen (e1,e2) -> Format.fprintf fmt "(%a when %a)"
                      print_expression e1
                      print_expression e2
  | CWhennot (e1,e2) -> Format.fprintf fmt "( %a whennot %a )"
                         print_expression e1
                         print_expression e2
  | CETuple el -> Format.fprintf fmt "(%a)"
                   print_expression_list el
  | CMerge (e1,e2,e3) ->
    Format.fprintf fmt  "(merge (%a) (%a) (%a))"
      print_expression e1
      print_expression e2
      print_expression e3
  | _ -> failwith "????"


let print_equation fmt (eq,vars) =
  Format.fprintf fmt "\t (%a : todo) = todo ;\n"
    print_pattern eq.cpattern
    (* print_ct eq.cclock *)
    (* print_expression eq.cexpression *)

let rec print_equations fmt (eqs,vars) =
  match eqs with
  | [] -> ()
  | [x] -> print_equation fmt (x,vars)
  | h::t -> Format.fprintf fmt "%a%a"
              print_equation (h,vars)
              print_equations (t,vars)

let print_node fmt (node,verbose) =
  let cs = node.cnode_clock in
  let (gv,t) = cs in
  if verbose then
  Format.fprintf fmt "node %a %a returns:%a = \n%a"
    print_pattern node.cname
    print_pattern node.cinputs
    print_pattern node.coutputs
    (* Minisimplclock.print_ct t *)
    print_equations (node.cequations,gv)
  else
    Format.fprintf fmt "%a \n%!"
      print_pattern node.cname
