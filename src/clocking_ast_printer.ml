
open Parsing_ast
open Parsing_ast_printer
open Clocking_ast
open Clocks

let print_equation fmt (eq,vars) =
  Format.fprintf fmt "\t (%a : %a) = %a ;\n"
    print_pattern eq.cpattern
    print_clock (eq.cclock,[])
    print_expression eq.cexpression

let rec print_equations fmt (eqs,vars) =
  match eqs with
  | [] -> ()
  | [x] -> print_equation fmt (x,vars)
  | h::t -> Format.fprintf fmt "%a%a" print_equation (h,vars) print_equations (t,vars)

let print_node fmt (node,verbose) =
  let cs = node.cnode_clock in
  let Forall(gv,gc,t) = cs in
  if verbose then
  Format.fprintf fmt "node %a %a returns:%a :: \027[32m%a\027[0m = \n%a"
    print_pattern node.cname
    print_pattern node.cinputs
    print_pattern node.coutputs
    Clocks.print_clock_scheme cs
    print_equations (node.cequations,gv)
  else
    Format.fprintf fmt "%a :: \027[32m%a\027[0m\n%!" print_pattern node.cname Clocks.print_clock_scheme cs
