open Astprinter
open Ast_clocks

let print_clock fmt c = 
  match c with 
  | Global -> Format.fprintf fmt " (global)"
  | Clock x -> Format.fprintf fmt " (%s)" x
  | Constant -> Format.fprintf fmt " (constant)"
(*)
let rec print_ctuple fmt l =
  match l with
  | [x,c] -> Format.fprintf fmt "%s %a" x.content print_clock c 
  | (h,c)::t -> Format.fprintf fmt "%s %a, %a" 
              h.content
              print_clock c 
              print_ctuple t
  | [] -> () 
*)

let print_cpattern fmt pp =
  match pp with
  | C_Simple (x,c) ->  Format.fprintf fmt "%s %a" x.content print_clock c 
  | C_List t -> ()

let rec print_cexpression fmt (e,c) =
  ( match e with 
  | C_Variable i -> print_ident fmt i
  | C_InfixOp (op, e1, e2) -> Format.fprintf fmt "(%a %a %a)"
      print_cexpression e1
      print_infop op
      print_cexpression e2
  | C_PrefixOp (op, e1) -> 
      Format.fprintf fmt "(%a %a)"
      print_preop op
      print_cexpression e1
  | C_Value v -> print_value fmt v 
  | C_When (e,i) -> Format.fprintf fmt " %a when %a "
      print_cexpression e
      print_ident i
  | C_Unit -> Format.fprintf fmt " () "
  | _ -> () ) ; (print_clock fmt c) 

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
  Format.fprintf fmt  "node %s %a returns %a ; \nlet \n%a\ntel \n "
    n.c_name.content
    print_io n.c_inputs 
    print_io n.c_outputs 
    print_cequations n.c_equations
