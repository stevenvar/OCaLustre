open Sequential_ast
open Parsing_ast_printer
open Parsing_ast

let rec print_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "(%a)" (print_list print_pattern) t
  | PUnit -> Format.fprintf fmt "()"
  | Typed (p,s) -> Format.fprintf fmt "(%a:%s)" print_pattern p s

let string_of_pattern p =
  match p.p_desc with
  | Ident i -> i
  | _ -> assert false


let rec print_s_ident_list fmt l =
  match l with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a"  print_ident x
  | h::t -> Format.fprintf fmt "%a,"  print_ident h; print_s_ident_list fmt t

let rec print_s_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "(%a)" (print_list print_s_pattern) t
  | PUnit -> Format.fprintf fmt "()"
  | Typed (p,s) -> Format.fprintf fmt "(%a:%s)" print_s_pattern p s

let rec print_s_tuple fmt l =
  match l with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a"  print_s_pattern x
  | h::t -> Format.fprintf fmt "%a,%a"
              print_s_pattern h
              print_s_tuple t

let rec print_s_expression fmt (exp,name) =
  let rec print_expression_list fmt el =
    match el with
    | [] -> ()
    | e::t -> Format.fprintf fmt "%a %a"
                print_s_expression (e,name)
                print_expression_list t
  in
  let print_s_preop fmt op =
    match op with
    | S_Not -> Format.fprintf fmt "not "
    | S_Neg -> Format.fprintf fmt "-"
    | S_Negf -> Format.fprintf fmt "-."
  in
  let print_s_infop fmt op =
    match op with
    | S_Diff -> Format.fprintf fmt "<>"
    | S_Equals -> Format.fprintf fmt "="
    | S_Plus -> Format.fprintf fmt "+"
    | S_Times -> Format.fprintf fmt "*"
    | S_Div -> Format.fprintf fmt "/"
    | S_Minus -> Format.fprintf fmt "-"
    | S_Minusf -> Format.fprintf fmt "-."
    | S_Divf -> Format.fprintf fmt "/."
    | S_Plusf -> Format.fprintf fmt "+."
    | S_Timesf -> Format.fprintf fmt "*."
    | S_Inf -> Format.fprintf fmt "<"
    | S_Infe -> Format.fprintf fmt "<="
    | S_Sup -> Format.fprintf fmt ">"
    | S_Supe -> Format.fprintf fmt ">="
    | S_Or -> Format.fprintf fmt "||"
    | S_And -> Format.fprintf fmt "&&"
    | S_Mod -> Format.fprintf fmt "mod"
  in
  let rec print_s_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 print_s_expression (e,name)
    | e::tl -> Format.fprintf fmt "%a,%a"
                 print_s_expression (e,name)
                 print_s_expressions tl
  in
  let rec print_s_list fmt l =
    match l with
    | [] -> Format.fprintf fmt "()"
    | [x] -> Format.fprintf fmt "%a"  print_s_expression (x,name)
    | h::t -> Format.fprintf fmt "%a %a"
                print_s_expression (h,name)
                print_s_list t
  in
  match exp.s_e_desc with
  | S_Magic -> Format.fprintf fmt "Obj.magic ()"
  | S_Value c -> print_value fmt c
  | S_Variable v ->  Format.fprintf fmt "%s" v
  | S_Ref s -> Format.fprintf fmt "state.%a_%s" print_pattern name s
  | S_RefDef e -> Format.fprintf fmt "ref %a" print_s_expression (e,name)
  | S_InfixOp (op,e1,e2) -> Format.fprintf fmt "%a %a %a"
                              print_s_expression (e1,name)
                              print_s_infop op
                              print_s_expression (e2,name)
  | S_PrefixOp (op,e) -> Format.fprintf fmt "%a%a"
                           print_s_preop op
                           print_s_expression (e,name)
  | S_Alternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                  print_s_expression (e1,name)
                                  print_s_expression (e2,name)
                                  print_s_expression (e3,name)
  | S_Unit -> Format.fprintf fmt "()"
  | S_Application_init (i,_n,el) -> Format.fprintf fmt "%s %a"
                                      i
                                      print_expression_list el
  | S_Application (i,_n,el) ->
    Format.fprintf fmt "%s %a"
      i
      print_expression_list el
  | S_Field (e,s) -> Format.fprintf fmt "%a.%s"
                       print_s_expression (e,name)
                       s
  | S_List el -> Format.fprintf fmt "%a"
                   print_s_list el
  | S_Call (f,e) -> Format.fprintf fmt "%s %a"
                      f
                      print_s_expression (e,name)
  | S_Constr s -> Format.fprintf fmt "%s" s
  | S_ETuple el -> Format.fprintf fmt "%a"
                     print_s_expressions el


let print_s_equations fmt (el,name) =
  let print_s_equation fmt (e,name) =
    Format.fprintf fmt "let %a = %a in \n"
      print_s_pattern e.s_pattern
      print_s_expression (e.s_expression,name)
  in
  List.iter (fun x -> print_s_equation fmt (x,name)) el

let rec print_s_inputs fmt ins =
  match ins with
  | [] -> Format.fprintf fmt "()"
  | [x] -> Format.fprintf fmt "%a"  print_ident x
  | x::xs -> Format.fprintf fmt "%a %a" print_ident x print_s_inputs xs

let rec print_pres fmt (pres,name) =
  match pres with
  | [] -> ()
  | [x] -> Format.fprintf fmt "pre_%a" print_ident x
  | x::xs -> Format.fprintf fmt "pre_%a ; %a"
               print_ident x
               print_pres (xs,name)

let rec print_outs fmt (outs,name) =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a_out_%a"
             print_pattern name
             print_ident x
  | x::xs -> Format.fprintf fmt "%a_out_%a ; %a"
               print_pattern name
               print_ident x
               print_outs (xs,name)


let rec print_calls fmt (calls,name) =
  match calls with
  | [] -> ()
  | [x] -> Format.fprintf fmt "%a_state" print_ident x
  | x::xs -> Format.fprintf fmt "%a_state %a"
               print_ident x
               print_calls (xs,name)


let rec print_calls_next fmt (calls,name) =
  match calls with
  | [] -> ()
  | [x] -> Format.fprintf fmt "state.%a_%a_state <- %a_state"
             print_pattern name
             print_ident x
             print_ident x

  | x::xs -> Format.fprintf fmt "state.%a_%a_state <- %a_state;\n%a"
               print_pattern name
               print_ident x
               print_ident x
               print_calls_next (xs,name)


let rec print_pres_next fmt (pres,name) =
  match pres with
  | [] -> ()
  | [x] -> Format.fprintf fmt "pre_%a <- %a;"
             print_ident x
             print_ident x

  | x::xs -> Format.fprintf fmt "pre_%a <- %a;\n%a"
               print_ident x
               print_ident x
               print_pres_next (xs,name)

let print_outs_next fmt (outs,name) =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "state.%a_out_%a <- %a"
             print_pattern name
             print_ident x
             print_ident x

  | x::xs -> Format.fprintf fmt "state.%a_out_%a <- %a;\n%a"
               print_pattern name
               print_ident x
               print_ident x
               print_calls_next (xs,name)

let print_s_state_next fmt (s,name) =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> (name^"_pre_"^s,"pre_"^s)) s.pres in
  let outs = List.map (fun s -> (name^"_out_"^s,s)) s.outs in
  let l = pres@outs in
  let rec loop fmt l =
    match l with
    | [] -> ()
    | [(x,y)] -> Format.fprintf fmt "state.%s <- %s " x y
    | (x,y)::xs -> Format.fprintf fmt "state.%s <- %s ;\n%a"
                     x
                     y
                     loop xs
  in
  loop fmt l

let print_s_state fmt (s,name) =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> (name^"_pre_"^s,s)) s.pres in
  let calls = List.map (fun s -> (name^"_"^s^"_state",s^"_state")) s.calls in
  let outs = List.map (fun s -> (name^"_out_"^s,s)) s.outs in
  let l = pres@calls@outs in
  let rec loop fmt l =
    match l with
    | [] -> ()
    | [(x,y)] -> Format.fprintf fmt "%s = %s " x y
    | (x,y)::xs -> Format.fprintf fmt "%s = %s ; %a"
                     x
                     y
                     loop xs
  in
  loop fmt l


let rec print_s_outs_next fmt (outs,name) =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "state.%a_%a <- %a"
             print_pattern name
             print_ident x
             print_ident x
  | x::xs -> Format.fprintf fmt "state.%a_%a <- %a;\n%a"
               print_pattern name
               print_ident x
               print_ident x
               print_s_outs_next (xs,name)

let print_s_zero fmt (f:s_fun) =
  Format.fprintf fmt "let %a_0 %a = \n%a{%a}\n"
    print_s_pattern f.s_name
    print_s_inputs f.s_inputs
    print_s_equations (f.s_eqs,f.s_name)
    print_s_state (f.s_state,f.s_name)
(* print_s_outs (f.s_outs,f.s_name) *)

let print_s_next fmt (f:s_fun) =
  Format.fprintf fmt "let %a_next state %a = \n%a%a\n"
    print_s_pattern f.s_name
    print_s_inputs f.s_inputs
    print_s_equations (f.s_eqs,f.s_name)
    print_s_state_next (f.s_state,f.s_name)
(* print_s_outs_next (f.s_outs,f.s_name) *)

let print_alphas fmt n =
  let rec loop fmt (k,n) =
    if k = n then Format.fprintf fmt "'%c" (Char.chr (97+k))
    else Format.fprintf fmt "'%c,%a" (Char.chr (97+k)) loop (k+1,n)
  in
  loop fmt (0,n)

let rec print_type_pres fmt (n,pres,name) =
  match pres with
  | [] -> ()
  | [x] -> Format.fprintf fmt "mutable %a_pre_%s :'%c"
             print_pattern name
             x
             (Char.chr (97+n))
  | h::t -> Format.fprintf fmt "mutable %a_pre_%s :'%c ; %a"
              print_pattern name
              h
              (Char.chr (97+n))
              print_type_pres ((n+1),t,name)


let rec print_type_calls fmt (n,calls,name) =
  match calls with
  | [] -> ()
  | [x] -> Format.fprintf fmt "mutable %a_%s_state :'%c;"
             print_pattern name
             x
             (Char.chr (97+n))
  | h::t -> Format.fprintf fmt "mutable %a_%s_state :'%c ; %a"
              print_pattern name
              h
              (Char.chr (97+n))
              print_type_calls ((n+1),t,name)

(* let rec print_type_outs fmt (n,outs,name) =
   match outs with
   | [] -> ()
   | [x] -> Format.fprintf fmt "mutable %a_out_%s :'%c;"
             print_pattern name
             x
             (Char.chr (97+n))
   | h::t -> Format.fprintf fmt "mutable %a_out_%s :'%c ; %a"
              print_pattern name
              h
              (Char.chr (97+n))
              print_type_outs ((n+1),t,name) *)

let print_type_state fmt (n,s,name) =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> name^"_pre_"^s) s.pres in
  let calls = List.map (fun s -> name^"_"^s^"_state") s.calls in
  let outs = List.map (fun s -> name^"_out_"^s) s.outs in
  let l = pres@calls@outs in
  let rec print_type fmt (n,l) =
    match l with
    | [] -> ()
    | [x] -> Format.fprintf fmt "mutable %s :'%c"
               x
               (Char.chr (97+n))
    | h::t -> Format.fprintf fmt "mutable %s :'%c ; %a"
                h
                (Char.chr (97+n))
                print_type ((n+1),t)
  in
  Format.fprintf fmt "%a"
    print_type (n,l)


let rec print_type_outs fmt (n,outs,name) =
  match outs with
  | [] -> ()
  | [x] -> Format.fprintf fmt "mutable %a_%s :'%c ; "
             print_pattern name
             x
             (Char.chr (97+n))
  | h::t -> Format.fprintf fmt "mutable %a_%s :'%c ; %a"
              print_pattern name
              h
              (Char.chr (97+n))
              print_type_outs ((n-1),t,name)

let print_type fmt node =
  let n = 0 in
  let state = node.s_next.s_state in
  let total = List.length (state.pres @ state.calls @ state.outs) -1 in
  Format.fprintf fmt "\ntype (%a) %a_state = {%a}"
    print_alphas total
    print_pattern node.s_name
    print_type_state (n,node.s_next.s_state,node.s_name)
(* print_type_outs (n,node.s_next.s_outs,node.s_name) *)

let print_s_node fmt node =
  Format.fprintf fmt "%a\n%a\n%a\n\n"
    print_type node
    print_s_zero node.s_zero
    print_s_next node.s_next
