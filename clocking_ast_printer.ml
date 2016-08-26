open Parsing_ast_printer
open Clocking_ast
open Clocking_ocl
open Parsing_ast

let rec print_clock fmt cl =
  let rec print_one_clock fmt c =
  match c with
  | Base -> Format.fprintf fmt "\x1b[32m<base>\x1b[0m"
  | On (ck,x)-> Format.fprintf fmt "\x1b[35m<%a \x1b[35mon %a>\x1b[0m"
                  print_one_clock ck
                  print_ident x
  in
  print_list print_one_clock fmt cl


let rec print_cpattern fmt { cp_desc ; cp_loc ; cp_clock } =
  match cp_desc with
  | CIdent i -> Format.fprintf fmt "(%s)%a" i print_clock cp_clock
  | CTuple t -> Format.fprintf fmt "(%a)%a" (print_list print_cpattern) t print_clock cp_clock

let print_io fmt l =
  print_list print_cpattern fmt l

let rec print_cexpression fmt { ce_desc ; ce_loc; ce_clock }=
  let rec print_cexpression_list fmt el =
    match el with
    | [] -> ()
    | [h] -> Format.fprintf fmt "%a" print_cexpression h
    | h::t -> Format.fprintf fmt "%a,%a"
                print_cexpression h
                print_cexpression_list t
  in
  match ce_desc with
  | CVariable i -> Format.fprintf fmt "%a%a"
                    print_ident i
                    print_clock ce_clock
  | CAlternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))%a"
      print_cexpression e1
      print_cexpression e2
      print_cexpression e3
      print_clock ce_clock
  | CInfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)%a"
      print_cexpression e1
      print_infop op
      print_cexpression e2
      print_clock ce_clock
  | CPrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)%a"
                           print_preop op
                           print_cexpression e1
                           print_clock ce_clock
  | CValue v -> print_value fmt v
  | CFby (v, e) -> Format.fprintf fmt "(%a fby %a)%a"
                     print_value v
                     print_cexpression e
                     print_clock ce_clock
  | CArrow (e1,e2) -> Format.fprintf fmt "(%a --> %a)%a"
                      print_cexpression e1
                      print_cexpression e2
                      print_clock ce_clock
  | CUnit -> Format.fprintf fmt "()"
  | CWhen (e,i) -> Format.fprintf fmt "( %a when %a )%a"
                    print_cexpression e
                    print_ident i
                    print_clock ce_clock
  | CPre e -> Format.fprintf fmt "(pre %a)%a"
                     print_cexpression e
                     print_clock ce_clock
  | CApplication (i,el) -> Format.fprintf fmt "(%s (%a))%a"
                             i
                             print_cexpression_list el
                             print_clock ce_clock
  | CETuple el -> Format.fprintf fmt "(%a)%a"
                             print_cexpression_list el
                             print_clock ce_clock


let print_cequation fmt e =
  Format.fprintf fmt  "  %a = %a;"
    print_cpattern e.cpattern
    print_cexpression e.cexpression

let rec print_cequations fmt le =
  match le with
  | [] -> ()
  | e::[] -> Format.fprintf fmt "%a"
               print_cequation e
  | e::tl -> Format.fprintf fmt "%a \n%a"
               print_cequation e
               print_cequations tl

let print_cnode fmt n =
  Format.fprintf fmt  "let_node %a ~i:%a ~o:%a = \n%a \n \n"
    print_cpattern n.cname
    print_io n.cinputs
    print_io n.coutputs
    print_cequations n.cequations
