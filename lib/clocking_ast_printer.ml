open Parsing_ast_printer
open Clocking_ast

open Parsing_ast


let rec print_tuple f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a * " f h ; print_tuple f fmt t
  | _ -> ()


let rec print_ct fmt = function

  | CVar {c_index = n ; c_value = CtUnknown } ->
    (*let name = try List.assoc n tvar_names
      with Not_found ->
        raise (ClockingBug ("Non generic variable :"^(string_of_int n)))
      in *) Format.fprintf Format.std_formatter "%d" n
  | CVar {c_index = n ; c_value = t} ->
    Format.fprintf Format.std_formatter "%a" print_ct t
  | Arrow (t1,t2) -> Format.fprintf fmt "%a -> %a" print_ct t1 print_ct t2
  | CTuple tl -> Format.fprintf fmt  "(%a)" (print_tuple print_ct) tl
  | On (x,i) ->  Format.fprintf fmt "%a on %d" print_ct x i.carr_index
  | Onnot (x,i) -> Format.fprintf fmt "%a on (not %d)" print_ct x i.carr_index
  | CtUnknown -> Format.fprintf fmt  "?"
  | Carrier c ->
    Format.fprintf fmt "(%d : %a)" c.carr_index print_ct c.carr_value


let rec print_clock fmt cl =
  match cl with
  | Clock_exp ct -> print_ct fmt ct
  | Clock_scheme sc -> ()


let rec print_cpattern fmt { cp_desc ; cp_loc ; cp_clock } =
  match cp_desc with
  | CkIdent i -> Format.fprintf fmt "(%s)<%a>" i print_clock cp_clock
  | CkTuple t -> Format.fprintf fmt "(%a)<%a>" (print_list print_cpattern) t print_clock cp_clock
  | CkPUnit -> Format.fprintf fmt "()"

let rec print_cexpression fmt { ce_desc ; ce_loc; ce_clock }=
  Format.fprintf fmt "%a<%a>" print_expression {e_desc = ce_desc ; e_loc = ce_loc } print_clock ce_clock 

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
    print_cpattern n.cinputs
    print_cpattern n.coutputs
    print_cequations n.cequations
