open Imperative_ast_printer
open Imperative_ast

let printwhyml_pre_equations fmt el =
  let printml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      print_pattern e.i_pattern
      printml_expression e.i_expression
  in
  List.iter (fun x -> printml_equation fmt x) el


let print_inv fmt i =
  match i with 
  | None -> ()
  | Some x -> Format.fprintf fmt "assert { %a }; \n" printml_expression x         

let printwhyml_post_equations fmt (el,inv) =
  let printml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      print_pattern e.i_pattern
      printml_expression e.i_expression
  in
  List.iter (fun x -> printml_equation fmt x) el; print_inv fmt inv


let print_assume fmt i =
  match i with 
  | None -> ()
  | Some x -> Format.fprintf fmt "assume { %a }; \n" printml_expression x    


let printwhyml_inits fmt (il,eql,inv) =
  let printml_init fmt { i_pattern = s ; i_expression = e} =
    begin match e with
      | x -> Format.fprintf fmt "let %a = %a in \n%a"
               print_pattern s
               printml_expression x
               printml_equations eql
    end
  in
  List.iter (fun i -> printml_init fmt i) il; print_inv fmt inv

let print_pre fmt p =
  match p with
  | None -> ()
  | Some x -> Format.fprintf fmt "requires { %a } \n" printml_expression x         


let print_post fmt (p,outs) =
  match p with
  | None -> ()
  | Some x -> Format.fprintf fmt "ensures { let %a = result in %a } \n" print_pattern outs printml_expression x         
  
let printwhyml_step fmt node =
  Format.fprintf fmt "let %s_step %a \n %a %a = \n%a%a%alet result = %a in \n%aresult \nin %s_step "
    node.i_name
    print_pattern node.i_inputs
    print_pre node.i_pre
    print_post (node.i_post,node.i_outputs)
    printwhyml_pre_equations node.i_step_fun.i_equations
    print_assume node.i_inv
    printml_updates node.i_step_fun.i_updates
print_pattern node.i_outputs
    printwhyml_post_equations (node.i_step_fun.i_equations,node.i_inv)
    
    node.i_name



let printwhyml_node fmt node =
  Format.fprintf fmt "let %s %a =\n%a%a"
    node.i_name
    print_pattern node.i_inputs
    printwhyml_inits (node.i_inits,node.i_step_fun.i_equations,node.i_inv)
    printwhyml_step node
