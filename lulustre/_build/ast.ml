type 'a node = {
 name : string ;
 inputs : string list;  
 outputs : string list; 
 equations : 'a equation list; 
}
and 
 'a equation = { 
 pattern : 'a pattern ; 
 expression : 'a expression; 
} 
and 
 'a pattern = ('a ident) list
and
 camlvalue 
and
  ('a, 'b) localized = { loc : 'a ; content : 'b }
and
 'a expression = ('a , exp_desc) localized 
and
 'a ident = ('a , string) localized
and
 exp_desc =   
  | Alternative of exp_desc * exp_desc * exp_desc 
  | InfixOp of inf_operator * exp_desc * exp_desc 
  | PrefixOp of pre_operator * exp_desc
  | Value of camlvalue  
  | Variable of string ident  
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
  | Opposed 
  | Pre 

let loc_default = "loc"

let mk_node name inputs outputs equations = { name ;  inputs ; outputs ; equations} 

let mk_ident v = { loc = loc_default ; content = v } 

let alternative e1 e2 e3 = Alternative (e1, e2, e3)

let (+) e1 e2 = InfixOp ( Plus , e1 , e2 ) 

let (-->) e1 e2 = InfixOp ( Arrow, e1, e2) 

let pre e1 = PrefixOp ( Pre , e1) 

let mk_variable v  = Variable (mk_ident v)

let equation = { pattern = [mk_ident "a"] ; expression = { loc = loc_default ; content = mk_variable "b"  } }  

let var1 = pre (mk_variable "v1")
let var2 = mk_variable "v2"
let var3 = mk_variable "v3"

let my_alternative = alternative var1 var2 var3

let equation2 = { pattern = [mk_ident "b"] ; expression = { loc = loc_default ; content = my_alternative } }  



let equations = [ equation ; equation2 ; equation ] 

let inputs = ["maman"; "papa";"bebe"]

let outputs = ["mamy" ; "papy" ; "toutou"]

let mynode = mk_node "monneud" inputs outputs equations

let rec print_list l = 
  match l with
  | h ::t when t <> []  -> (Printf.printf "%s , " h ; print_list t) 
  | s :: []             -> Printf.printf "%s" s 
  | _                  -> () 

let print_io n = Printf.printf "(";  print_list n ; Printf.printf ")"

let print_ident i = Printf.printf "%s" i.content  

let print_pattern pp = List.iter (fun e -> Printf.printf "%s" e.content) pp

let print_op op = 
  match op with 
  | Pre -> Printf.printf "pre "
  | _ -> ()

let rec print_expression e = match e with 
  | Variable i -> print_ident i 
  | Alternative (e1,e2,e3) -> Printf.printf "if "; print_expression e1 ; Printf.printf " then "; print_expression e2 ; Printf.printf " else ";  print_expression e3
  | PrefixOp (op, e1) -> print_op op ; print_expression e1
  | _ -> Printf.printf "()" 

let print_equation e = Printf.printf "\t"; print_pattern e.pattern ; Printf.printf "="; print_expression e.expression.content  

let print_equations le = Printf.printf "let \n"; List.iter (fun e -> print_equation e; Printf.printf "\n" ) le ; Printf.printf "tel \n "

let print_node n = Printf.printf "node %s " n.name; print_io n.inputs; Printf.printf " returns ";  print_io n.outputs ; Printf.printf "; \n" ;  print_equations n.equations 
													      
let _ = print_node mynode  
		     
