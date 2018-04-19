open Parsing_ast
open Parsing_ast_printer


let rec print_list f fmt l =
  match l with
  | [s] -> Format.fprintf fmt  "%a" f s
  | h :: t -> Format.fprintf fmt  "%a; " f h ; print_list f fmt t
  | _ -> ()


let print_infop fmt op =
  match op with
  | Equals -> Format.fprintf fmt "="
  | Plus -> Format.fprintf fmt "+"
  | Times -> Format.fprintf fmt "*"
  | Div -> Format.fprintf fmt "/"
  | Minus -> Format.fprintf fmt "-"
  | Diff -> Format.fprintf fmt "<>"
  | Plusf -> Format.fprintf fmt "+"
  | Timesf -> Format.fprintf fmt "*"
  | Minusf -> Format.fprintf fmt "-"
  | Divf -> Format.fprintf fmt "/"
  | Inf -> Format.fprintf fmt "<"
  | Infe -> Format.fprintf fmt "<="
  | Sup -> Format.fprintf fmt ">"
  | Supe -> Format.fprintf fmt ">="
  | Bor -> Format.fprintf fmt "or"
  | Band -> Format.fprintf fmt "and"
  | Mod -> Format.fprintf fmt "mod"

let rec print_pattern fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "%a" (print_list print_pattern) t
  | PUnit -> Format.fprintf fmt ""
  | Typed (p,s) -> Format.fprintf fmt "%a:%s"
                     print_pattern p
                     s


let rec print_name fmt p =
  match p.p_desc with
  | Ident i -> Format.fprintf fmt "%s" i
  | Tuple t -> Format.fprintf fmt "%a" (print_list print_pattern) t
  | PUnit -> Format.fprintf fmt ""
  | Typed (p,s) -> print_pattern fmt p


let rec print_expression fmt e =
  let rec print_expression_list fmt el =
    match el with
    | [] -> ()
    | [e] -> Format.fprintf fmt "%a" print_expression e
    | he::te -> Format.fprintf fmt "%a,%a" print_expression he print_expression_list te
  in
  match e.e_desc with
  | Array _ | Array_get _ | Imperative_update _ | Array_fold _ | Array_map _ -> failwith "todo"
  | Variable i -> Format.fprintf fmt "%a"
                    print_ident i
  | Alternative (e1,e2,e3) ->
    Format.fprintf fmt  "(if (%a) then (%a) else (%a))"
      print_expression e1
      print_expression e2
      print_expression e3
  | Application (i,num,e) ->
     Format.fprintf fmt "(%a (%a))"
                    print_ident i
                    print_expression e
  | Call (e) ->
     Format.fprintf fmt "(eval ...)"
  | InfixOp (op, e1, e2) ->
    Format.fprintf fmt "(%a %a %a)"
      print_expression e1
      print_infop op
      print_expression e2
  | Pre e -> Format.fprintf fmt "(pre %a)" print_expression e
  | PrefixOp (op, e1) -> Format.fprintf fmt "(%a %a)"
                           print_preop op
                           print_expression e1

  | Value v -> print_value fmt v
  | Arrow (e1,e2) -> Format.fprintf fmt "(%a -> %a)"
                       print_expression e1
                       print_expression e2
  | Fby (e1, e2) -> Format.fprintf fmt "(%a -> pre %a)"
                    print_expression e1
                    print_expression e2
  | Unit -> Format.fprintf fmt "()"
  | Clock e -> Format.fprintf fmt "%a" print_expression e
  | When (e1,e2) -> Format.fprintf fmt "( %a when %a )"
                    print_expression e1
                    print_expression e2
  | Whennot (e1,e2) -> Format.fprintf fmt "( %a whennot %a )"
                                      print_expression e1
                                      print_expression e2
  | ETuple el -> Format.fprintf fmt "(%a)"
                   print_expression_list el
  | Merge (e1,e2,e3) ->
    Format.fprintf fmt  "(merge (%a) (%a) (%a))"
    print_expression e1
    print_expression e2
    print_expression e3




let print_equation fmt e =
  Format.fprintf fmt  "  %a = %a;"
    print_name e.pattern
    print_expression e.expression

let rec print_equations fmt le =
  match le with
  | [] -> ()
  | e::[] -> Format.fprintf fmt "%a"
               print_equation e
  | e::tl -> Format.fprintf fmt "%a \n%a"
               print_equation e
               print_equations tl

let print_condition fmt cond =
  match cond with
  | Some x ->
    Format.fprintf fmt "\n --%%PROPERTY %a;" print_expression x;
  | None -> ()

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let split p =
  match p.p_desc with
  | Tuple t -> t
  | _ -> [p]

let remove_type x =
  match x.p_desc with
  | Typed (x,t) -> x
  | _ -> x

let rec print_local_vars fmt l =
  match l with
  | [] -> ()
  | x::xs -> Format.fprintf fmt "var %a;\n %a\n" print_pattern x
               print_local_vars xs


let rec to_string x =
  match x.p_desc with
  | Ident x -> x
  | PUnit -> "()"
  | Typed(p,t) -> to_string p
  | _ -> failwith "cannot make a string"

let rec remove_var x l =
  match l with
  | [] -> []
  | p::t ->
    let xname = to_string x in
    let name = to_string p in
    if xname = name then t else p::remove_var x t


let get_local_vars node =
  let vars =
    List.map (fun eq -> eq.pattern) node.equations
  in
  let ins = split node.inputs in
  let outs = split node.outputs in
  let vars = List.fold_left (fun acc x -> remove_var x acc) vars outs in
  List.fold_left (fun acc x -> remove_var x acc) vars ins

let print_node fmt node =
  Format.fprintf fmt "
node %a(%a) returns (%a)
 %alet
%a%a
 tel\n%!
" print_pattern node.name
    print_pattern node.inputs
    print_pattern node.outputs
    print_local_vars (get_local_vars node)
    print_equations node.equations
    print_condition node.inv

let to_lustre_file node =
  let name = !Location.input_name in
  let name = Filename.remove_extension name in
  let node_name = Tools.string_of_pattern node.name in
  let name = name^"_"^node_name^".ls" in
  (* let oc = open_out_gen [ Open_wronly; Open_creat ; Open_append] 0o640 name in *)
  let oc = open_out name in
  let fmt =  Format.formatter_of_out_channel oc in
  Format.printf "%a" print_node node;
  Format.fprintf fmt "%a" print_node node;
  close_out oc;
  Format.printf "File %s has been written for node %s. \n"
    name
    node_name
