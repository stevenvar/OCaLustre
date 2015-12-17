open Ast


let print_value fmt v = 
  match v with 
  | Integer i -> Format.fprintf fmt "%d" i
  | Float f -> Format.fprintf fmt "%f" f
  | Bool true  -> Format.fprintf fmt "true"
  | Bool false  -> Format.fprintf fmt "false"

let rec print_list f fmt l = 
  match l with
  | h ::t when t <> []-> (Format.fprintf fmt  "%a , " f h ; print_list f fmt t) 
  | s :: [] -> Format.fprintf fmt  "%a" f s 
  | _ -> () 

let print_io fmt n =
  Format.fprintf fmt "(%a)"
    (print_list (fun fmt (x,c) -> Format.fprintf fmt "%s <%s>" x.content c))  n

let print_ident fmt i = Format.fprintf fmt "%s" i.content  

let rec print_tuple fmt l =
  match l with
  | [x] -> Format.fprintf fmt "%s" x.content
  | h::t -> Format.fprintf fmt "%s," h.content; print_tuple fmt t
  | [] -> () 

let print_pattern fmt pp =
  match pp with
  | Simple x ->  Format.fprintf fmt "%s" x.content
  | List t -> print_tuple fmt t
    
let print_preop fmt op = 
  match op with
  | Pre -> Format.fprintf fmt  "pre "
  | Not -> Format.fprintf fmt "not "


let print_infop fmt op =
  match op with
  | Equals -> Format.fprintf fmt " = " 
  | Plus -> Format.fprintf fmt " + "
  | Times -> Format.fprintf fmt " * "
  | Div -> Format.fprintf fmt " / "
  | Minus -> Format.fprintf fmt " - "
  | Plusf -> Format.fprintf fmt " +. "
  | Timesf -> Format.fprintf fmt " *. "
  | Divf -> Format.fprintf fmt " /. "
  | Minusf -> Format.fprintf fmt " -. "
  | Arrow -> Format.fprintf fmt " -> "
  | Diff -> Format.fprintf fmt " <> "
  | Great -> Format.fprintf fmt " > "
  | Less -> Format.fprintf fmt " < "
  | Lesse -> Format.fprintf fmt " <= "
  | Greate -> Format.fprintf fmt " >= "
  | And -> Format.fprintf fmt " && "
  | Or -> Format.fprintf fmt " || "

let rec print_expression fmt e =
  let print_expressions fmt el =
    List.iter (fun x -> print_expression fmt x) el
  in 
  let print_application fmt (i,el) =
    Format.fprintf fmt "%s %a"
      i.content
      print_expressions el
  in
  let rec print_expression_list fmt el =
    match el with
    | [x] -> print_expression fmt x
    | h::t -> Format.fprintf fmt "%a,%a"
                print_expression h
                print_expression_list t 
    | [] -> ()
  in
  match e with 
  | Variable i -> print_ident fmt i
  | Tuple t -> print_expression_list fmt t
  | Ref i -> print_ident fmt i
  | Alternative (e1,e2,e3) ->
    Format.fprintf fmt  "if %a then %a else %a" 
      print_expression e1 
      print_expression e2 
      print_expression e3
  | InfixOp (op, e1, e2) ->
    Format.fprintf fmt "%a %a %a"
      print_expression e1
      print_infop op
      print_expression e2
  | Application (i, el) -> print_application fmt (i,el)
  | Application_init (i, el) -> print_application fmt (i,el)
  | PrefixOp (op, e1) -> print_preop fmt op ; print_expression fmt e1
  | Value v -> print_value fmt v 
  | Call e ->
    Format.fprintf fmt "call (%a)"
      Pprintast.expression e
  | When (e,i) ->
    Format.fprintf fmt "%a when %a"
      print_expression e
      print_ident i
  | Current e ->
    Format.fprintf fmt "current (%a)"
      print_expression e
  | Current_init e -> Format.fprintf fmt "curr_%a"
                        print_expression e
  | Unit -> Format.fprintf fmt "()"
  

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
  Format.fprintf fmt  "node %s %a returns %a ; \nlet \n%a\ntel \n "
    n.name.content
    print_io n.inputs 
    print_io n.outputs 
    print_equations n.equations
