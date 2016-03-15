open Ast



let print_value fmt v = 
  match v with 
  | Integer i -> Format.fprintf fmt "%d" i

let rec print_list f fmt l = 
  match l with
  | h ::t when t <> []-> (Format.fprintf fmt  "%a, " f h ; print_list f fmt t) 
  | s :: [] -> Format.fprintf fmt  "%a" f s 
  | _ -> () 

let print_ident fmt i = Format.fprintf fmt "%s" i.content  

let rec print_clock fmt c =
  match c with
  | Base -> Format.fprintf fmt "\x1b[32m<base>\x1b[0m"
  | On (ck,x)-> Format.fprintf fmt "\x1b[35m<%a \x1b[35m on %a>\x1b[0m"
                  print_clock ck
                  print_ident x


let print_io fmt l =
  let print_one fmt x =
    Format.fprintf fmt "%s"
      x.content
  in
  Format.fprintf fmt "(%a)"
    (print_list (fun fmt io -> print_one fmt io)) l


let rec print_tuple fmt l =
  match l with
  | [x] -> Format.fprintf fmt "%s" x.content
  | h::t -> Format.fprintf fmt "%s," h.content; print_tuple fmt t
  | [] -> () 

let print_pattern fmt p =
  Format.fprintf fmt "(%s)"
    p.content



let print_preop fmt op = 
  match op with
  | Not -> Format.fprintf fmt "not "


let print_infop fmt op =
  match op with
  | Equals -> Format.fprintf fmt "=" 
  | Plus -> Format.fprintf fmt "+"
  | Times -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Minus -> Format.fprintf fmt "-"
  | Diff -> Format.fprintf fmt "<>"



let rec print_expression fmt (e,c) =
  match e with 
  | Variable i -> Format.fprintf fmt "%a%a"
                    print_ident i
                    print_clock c 
  | Alternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))%a" 
      print_expression e1 
      print_expression e2 
      print_expression e3
      print_clock c
  | InfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)%a"
      print_expression e1
      print_infop op
      print_expression e2
      print_clock c
  | PrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)%a"
                           print_preop op
                           print_expression e1
                           print_clock c
  | Value v -> print_value fmt v
  | Fby (v, e) -> Format.fprintf fmt "(%a fby %a)%a"
                    print_value v
                    print_expression e
                    print_clock c
  | Unit -> Format.fprintf fmt "()"
  | When (e,i) -> Format.fprintf fmt "( %a when %a )%a"
                    print_expression e
                    print_ident i
                    print_clock c


let print_equation fmt e =
  Format.fprintf fmt  "  %a = %a;"
    print_pattern e.pattern
    print_expression e.expression

let rec print_equations fmt le =
  match le with
  | [] -> ()
  | e::[] -> Format.fprintf fmt "%a"
               print_equation e 
  | e::tl -> Format.fprintf fmt "%a \n%a"
               print_equation e
               print_equations tl 

let print_node fmt n =
  Format.fprintf fmt  "node %s %a %a = \n%a \n \n"
    n.name.content
    print_io n.inputs 
    print_io n.outputs 
    print_equations n.equations
