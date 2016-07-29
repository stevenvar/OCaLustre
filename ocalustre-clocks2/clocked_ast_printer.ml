open Ast
open Ast_printer
open Clocked_ast

let rec print_clock fmt c =
  match c with
  | Base -> Format.fprintf fmt "\x1b[32m<base>\x1b[0m"
  | On (ck,x)-> Format.fprintf fmt "\x1b[35m<%a \x1b[35mon %a>\x1b[0m"
                  print_clock ck
                  print_ident x
                  
let print_io fmt l =
  let print_one fmt (x,c) =
    Format.fprintf fmt "%s%a"
      x.content
      print_clock c
  in
  Format.fprintf fmt "(%a)"
    (print_list (fun fmt io -> print_one fmt io)) l


let print_cpattern fmt (p,c) =
  Format.fprintf fmt "(%s %a)"
    p.content
    print_clock c

let print_preop fmt op = 
  match op with
  | Not -> Format.fprintf fmt "not "



let rec print_cexpression fmt (e,c) =
  let rec print_cexpression_list fmt el =
    match el with
    | [] -> () 
    | [h] -> Format.fprintf fmt "%a" print_cexpression h
    | h::t -> Format.fprintf fmt "%a,%a"
                print_cexpression h
                print_cexpression_list t
  in 
  match e with 
  | CVariable (i,c) -> Format.fprintf fmt "%a%a"
                    print_ident i
                    print_clock c 
  | CAlternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))%a" 
      print_cexpression e1 
      print_cexpression e2 
      print_cexpression e3
      print_clock c
  | CInfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)%a"
      print_cexpression e1
      print_infop op
      print_cexpression e2
      print_clock c
  | CPrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)%a"
                           print_preop op
                           print_cexpression e1
                           print_clock c
  | CValue v -> print_value fmt v
  | CFby (v, e) -> Format.fprintf fmt "(%a fby %a)%a"
                    print_value v
                    print_cexpression e
                    print_clock c
  | CUnit -> Format.fprintf fmt "()"
  | CWhen (e,(i,ck)) -> Format.fprintf fmt "( %a on %a )%a"
                    print_cexpression e
                    print_ident i
                    print_clock c
  | CCurrent e -> Format.fprintf fmt "(current %a)%a"
                    print_cexpression e
                    print_clock c
  | CPre (v,ck) -> Format.fprintf fmt "(pre %s)%a"
                     v.content
                     print_clock c
  | CApplication (i,el) -> Format.fprintf fmt "(%s (%a))%a"
                             i.content
                             print_cexpression_list el
                             print_clock c




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
  Format.fprintf fmt  "let_node %s ~inf:%a ~outf:%a = \n%a \n \n"
    n.cname.content
    print_io n.cinputs 
    print_io n.coutputs 
    print_cequations n.cequations
