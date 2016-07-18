open Ast_printer
open Ast_clock


(*
let rec print_ctuple fmt l =
  match l with
  | [x,c] -> Format.fprintf fmt "%s %a" x.content print_clock c 
  | (h,c)::t -> Format.fprintf fmt "%s %a, %a" 
              h.content
              print_clock c 
              print_ctuple t
  | [] -> () 
*)


let print_cpattern = print_pattern


let rec print_cexpression fmt (e,c) =
  match e with 
  | C_Variable (i,c) -> Format.fprintf fmt "%a%a" 
            print_ident i 
            print_clock c
  | C_InfixOp (op, e1, e2) -> Format.fprintf fmt "(%a %a %a)%a"
      print_cexpression e1
      print_infop op
      print_cexpression e2
      print_clock c
  | C_PrefixOp (op, e1) -> 
      Format.fprintf fmt "(%a %a)%a"
      print_preop op
      print_cexpression e1
      print_clock c
  | C_Value v -> print_value fmt v 
  | C_Unit -> Format.fprintf fmt "()"
  | C_Alternative (e1,e2,e3) -> 
      Format.fprintf fmt "(if %a then %a else %a )%a"
      print_cexpression e1
      print_cexpression e2
      print_cexpression e3
      print_clock c
  | C_Fby (v,e) ->
    Format.fprintf fmt "(%a fby %a)%a"
      print_value v
      print_cexpression e
      print_clock c


let print_cequation fmt e =
  Format.fprintf fmt  "  %a = %a;"
    print_cpattern e.c_pattern
    print_cexpression e.c_expression

let rec print_cequations fmt le =
  match le with
  | [] -> ()
  | e::[] -> Format.fprintf fmt "%a"
               print_cequation e 
  | e::tl -> Format.fprintf fmt "%a \n%a"
               print_cequation e
               print_cequations tl 


 
let print_cnode fmt n =
  Format.fprintf fmt  "\n node %s %a returns %a = \n%a\n \n "
    n.c_name.content
    print_io n.c_inputs 
    print_io n.c_outputs 
    print_cequations n.c_equations
