open Ast_printer
open Ast_clock

let print_clock fmt c = 
  match c with 
  | Global -> Format.fprintf fmt "(global)"
  | Clock x -> Format.fprintf fmt "(%s)" x
  | Constant -> Format.fprintf fmt "(constant)"
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
  let print_cexpressions fmt el =
    List.iter (fun x -> print_cexpression fmt x) el
  in 
  let print_application fmt (i,el) =
    Format.fprintf fmt "%a (%a)"
      print_ident i 
      print_cexpressions el
    in
  match e with 
  | C_Variable i -> Format.fprintf fmt "%a %a " 
            print_ident i 
            print_clock c
  | C_Ref i -> Format.fprintf fmt "%a %a " 
            print_ident i 
            print_clock c
  | C_InfixOp (op, e1, e2) -> Format.fprintf fmt "(%a %a %a)"
      print_cexpression e1
      print_infop op
      print_cexpression e2
  | C_PrefixOp (op, e1) -> 
      Format.fprintf fmt "(%a %a)"
      print_preop op
      print_cexpression e1
  | C_Value v -> print_value fmt v 
  | C_When (e,i) -> Format.fprintf fmt "(%a when %a)"
      print_cexpression e
      print_ident i
  | C_Unit -> Format.fprintf fmt "()"
  | C_Alternative (e1,e2,e3) -> 
      Format.fprintf fmt "(if %a then %a else %a )"
      print_cexpression e1
      print_cexpression e2
      print_cexpression e3
  | C_Application_init (i, cel) ->  
    print_application fmt (i,cel) 
  | C_Application (i, cel) ->  
    print_application fmt (i,cel) 
  | C_Call e -> Format.fprintf fmt "call (%a) (global)"
        Pprintast.expression e


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
