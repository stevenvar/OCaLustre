open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast
      
let checkname_pattern n =
  match n.ppat_desc with
    Ppat_var sl -> sl.txt
  | _ -> failwith "this is not a pattern"

let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> mk_ident ~loc s
  | _ -> failwith "this is not an expression" 
   
let rec mk_expr e =
  match e with
    [%expr [%e? e1] + [%e? e2] ] -> mk_expr e1 + mk_expr e2
  | [%expr [%e? e1] * [%e? e2] ] -> mk_expr e1 * mk_expr e2
  | [%expr [%e? e1] - [%e? e2] ] -> mk_expr e1 - mk_expr e2
  | [%expr [%e? e1] / [%e? e2] ] -> mk_expr e1 / mk_expr e2
  | [%expr pre [%e? e1] ] -> pre (mk_expr e1)
  | [%expr [%e? e1] --> [%e? e2] ] -> (mk_expr e1) --> (mk_expr e2) 
  | [%expr if [%e? e1] then [%e? e2] else [%e? e3] ] ->
    alternative (mk_expr e1) (mk_expr e2) (mk_expr e3)
  | [%expr true ] -> Value e
  | [%expr false ] -> Value e
  | { pexp_desc = Pexp_constant c ; pexp_loc ; pexp_attributes } ->
    Value (e)
  | { pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
      pexp_loc ;
      pexp_attributes } ->
    mk_variable v 
  | _ -> failwith "wrong expression syntax after := " 
		  
let mk_equation eq =
  match eq with
    [%expr [%e? p] := [%e? e] ] ->
    {pattern= [checkname_ident p]; expression = mk_expr e}
  | _ -> failwith "wrong equation syntax" 

let rec mk_equations eqs =
  match eqs with
    [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e] 
 
    
let mk_node name equations =
  { name=checkname_pattern name ;
    inputs=[];
    outputs=[];
    equations= mk_equations equations }
		     
let lustre_mapper argv =
  { default_mapper with
    structure_item = fun mapper str ->
      match str.pstr_desc with
	Pstr_extension (({txt="node";_},PStr [s]),_) ->
	begin match s.pstr_desc with
		Pstr_value (_,[v]) ->
                  let _node = mk_node (v.pvb_pat) (v.pvb_expr) in
                  print_node Format.std_formatter _node; 
	        [%stri let () = () ]
	      | _ -> failwith "syntax error in node" 
	end
      | x -> default_mapper.structure_item mapper str
  }

let () = register "lustre" lustre_mapper 
