open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast


(* check if the pattern is a variable *)
let checkname_pattern n =
  match n.ppat_desc with
    Ppat_var sl -> {loc=sl.loc ; content=sl.txt }
  | _ -> failwith "this is not a pattern"

(* check if the name is an ident  *)
let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> mk_ident ~loc s
  | _ -> failwith "this is not an expression" 

(* transform expressions to node of the ocalustre AST *)
let rec mk_expr e =
  match e with
  | [%expr [%e? e1] + [%e? e2] ] -> mk_expr e1 + mk_expr e2
  | [%expr [%e? e1] * [%e? e2] ] -> mk_expr e1 * mk_expr e2
  | [%expr [%e? e1] - [%e? e2] ] -> mk_expr e1 - mk_expr e2
  | [%expr [%e? e1] / [%e? e2] ] -> mk_expr e1 / mk_expr e2
  | [%expr pre [%e? e1] ] -> pre (mk_expr e1)
  | [%expr [%e? e1] --> [%e? e2] ] -> (mk_expr e1) --> (mk_expr e2) 
  | [%expr if [%e? e1] then [%e? e2] else [%e? e3] ] ->
    alternative (mk_expr e1) (mk_expr e2) (mk_expr e3)
  | [%expr true ] -> Value e
  | [%expr false ] -> Value e
  | [%expr not [%e? e1] ] -> not (mk_expr e1)
  | { pexp_desc = Pexp_constant c;
      pexp_loc ;
      pexp_attributes } ->
    Value (e)
  | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
     pexp_loc ;
     pexp_attributes} ->
    mk_variable v 
  | _ -> failwith "wrong expression syntax after := " 

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
    [%expr [%e? p] := [%e? e] ] ->
    {pattern= [checkname_ident p];
     expression = mk_expr e}
  | _ -> failwith "wrong equation syntax" 

(* creates list of equations nodes in the AST *)
let rec mk_equations eqs =
  match eqs with
  | [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e] 

(* check that the I/O are tuples and returns a list of corresponding idents *) 
let checkio body =
  match body with
  | [%expr fun () -> [%e? body] ] -> ( [], body)
  | [%expr fun [%p? inputs] -> [%e? body] ] ->
    begin match inputs.ppat_desc with
      | Ppat_var s -> ([mk_ident ~loc:s.loc s.txt], body ) 
      | Ppat_tuple l -> (List.map checkname_pattern l, body)
      | _ -> raise Location.
                     (Error
                         (error
                             ~loc:inputs.ppat_loc
                             "Error: Syntax error in i/o definition"
                         )
                     ) 
    end 
  | _ -> raise 
    Location.(Error(error ~loc:body.pexp_loc "Error: Syntax error in node"))

let rec get_idents e =
  match e with 
  | Variable i -> [i] 
  | Alternative (e1,e2,e3) ->
    get_idents e1 @ 
    get_idents e2 @
    get_idents e3 
  | InfixOp (op, e1, e2) ->
      get_idents e1 @
      get_idents e2
  | PrefixOp (op, e1) ->
    begin match op with
      | Pre -> []
      | _ -> get_idents e1
    end
  | Value v -> []

let print_bool b =
  match b with
  | true -> print_string "true"
  | false -> print_string "false"
               
let elem_exists_in_list k l =
  List.fold_left (fun b x -> b || x.content = k.content) false l

let list_exists_in_list l1 l2 =
  List.fold_left (fun b x -> b && elem_exists_in_list x l2) true l1  

let rec check_dependencies_list acc equations =
  let check_dependency acc eq =
    if list_exists_in_list (get_idents eq.expression) acc then
      ((acc@eq.pattern),true)
    else
      (acc,false)
  in
  List.fold_left
    (fun (a,b) ll ->
       let dep = check_dependency acc ll in 
       (a@fst dep,b &snd dep)
    )
    ([],true) equations
 
    
let print_dependencies fmt node =
  let eq_dep fmt eq =
    Format.fprintf fmt "%a depends on %a \n"
      print_io eq.pattern
      print_io (get_idents eq.expression)
  in
  List.map (eq_dep fmt) node.equations
         
    

(* creates a node "lustre node" in the AST *)
let mk_node name body =
  let name = checkname_pattern name in
  let inputs, body = checkio body in
  let outputs, body = checkio body in
  let equations = mk_equations body in
  {
    name;
    inputs;
    outputs;
    equations
  }

(* maps structures of the form : 

   let%node NOM (IN1,IN2,...) (OUT1, OUT2, ...) = 
    IN1 := OUT1;
    ...
  
*)
let lustre_mapper argv =
  { default_mapper with
    structure_item = fun mapper str ->
      match str.pstr_desc with
      | Pstr_extension (({txt="node";_},PStr [s]),_) ->
        begin match s.pstr_desc with
          | Pstr_value (_,[v]) ->
            let _node = mk_node (v.pvb_pat) (v.pvb_expr) in
            print_dependencies Format.std_formatter _node;
            print_bool (snd (check_dependencies_list _node.inputs _node.equations));
            print_node Format.std_formatter _node;  
            [%stri let () = () ]
          | _ -> raise 
                   Location.
                     (Error
                        (error
                           ~loc:s.pstr_loc
                           "Error: Syntax error in node"
                        )
                     )
        end
      | x -> default_mapper.structure_item mapper str
  }

let () = register "lustre" lustre_mapper 
