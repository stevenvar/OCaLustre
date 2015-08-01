type node = {
 name : ident ;
 inputs : ident list;  
 outputs : ident list; 
 equations : equation list; 
}
and 
 equation = { 
 pattern : pattern ; 
 expression : expression; 
} 
and 
 pattern = (ident) list
and
  constant =
  Parsetree.expression 
and
 expression = exp_desc 
and
  ident = {
  loc : Location.t;
  content :  string
}
and
 exp_desc =   
  | Alternative of exp_desc * exp_desc * exp_desc 
  | InfixOp of inf_operator * exp_desc * exp_desc 
  | PrefixOp of pre_operator * exp_desc
  | Value of constant 
  | Variable of ident   
and
 inf_operator = 
  | Plus
  | Minus
  | Times
  | Div
  | Arrow 
and
 pre_operator = 
  | Not
  | Pre 

let mk_node name inputs outputs equations =
  {
   name ;
   inputs ;
   outputs ;
   equations
  } 

let loc_default = Location.none
					      
let mk_ident ?(loc=loc_default) v = { loc ; content = v } 

let alternative e1 e2 e3 = Alternative (e1, e2, e3)

let (+) e1 e2 = InfixOp ( Plus , e1 , e2 ) 

let ( * ) e1 e2 = InfixOp ( Times , e1 , e2)

let ( - ) e1 e2 = InfixOp ( Minus, e1, e2)

let ( / ) e1 e2 = InfixOp (Div, e1, e2) 

let (-->) e1 e2 = InfixOp ( Arrow, e1, e2) 

let pre e1 = PrefixOp ( Pre , e1) 

let not e1 = PrefixOp ( Not , e1) 
  
let mk_variable v  = Variable (mk_ident v)

let rec print_list f fmt l = 
  match l with
  | h ::t when t <> []-> (Format.fprintf fmt  "%a , " f h ; print_list f fmt t) 
  | s :: [] -> Format.fprintf fmt  "%a" f s 
  | _ -> () 

let print_io fmt n =
  Format.fprintf fmt  "(%a)"
    (print_list (fun fmt x -> Format.fprintf fmt "%s" x.content))  n

let print_ident fmt i = Format.fprintf fmt "%s" i.content  

let print_pattern fmt pp =
  List.iter (fun e -> Format.fprintf fmt "%s" e.content) pp

let print_preop fmt op = 
  match op with
  | Pre -> Format.fprintf fmt  "pre "
  | Not -> Format.fprintf fmt "not "


let print_infop fmt op =
  match op with
  | Plus -> Format.fprintf fmt " + "
  | Times -> Format.fprintf fmt " * "
  | Div -> Format.fprintf fmt " / "
  | Minus -> Format.fprintf fmt " - "
  | Arrow -> Format.fprintf fmt " -> "
  

let rec print_expression fmt e =
  match e with 
  | Variable i -> print_ident fmt i 
  | Alternative (e1,e2,e3) ->
    Format.fprintf fmt  "if %a then %a else %a" 
    print_expression e1 
    print_expression e2 
    print_expression e3
  | InfixOp (op, e1, e2) -> Format.fprintf fmt "%a %a %a"
      print_expression e1
      print_infop op
      print_expression e2
  | PrefixOp (op, e1) -> print_preop fmt op ; print_expression fmt e1
  | Value v -> Pprintast.expression fmt v 
  

let print_equation fmt e =
  Format.fprintf fmt  "  %a = %a"
    print_pattern e.pattern
    print_expression e.expression

let rec print_equations fmt le =
  match le with
  | e::[] -> Format.fprintf fmt "%a" print_equation e
  | e::ll ->
    Format.fprintf fmt "%a \n" print_equation e;
    print_equations fmt ll
  | _ -> ()
 
let print_node fmt n =
  Format.fprintf fmt  "node %s %a returns %a ; \nlet \n%a\ntel \n "
    n.name.content
    print_io n.inputs 
    print_io n.outputs 
    print_equations n.equations

