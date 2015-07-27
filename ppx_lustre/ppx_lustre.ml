open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast
      
let checkname n =
  match n.ppat_desc with
    Ppat_var sl -> sl.txt
  | _ -> failwith "oui" 

let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> mk_ident ~loc s
  | _ -> failwith "pas expr" 
   
let mk_expr e = assert false  
		  
let mk_equation eq =
  match eq with
    [%expr [%e? p] := [%e? e] ] -> {pattern= [checkname_ident p]; expression = mk_expr e}
  | _ -> failwith "syntax error" 

let rec mk_equations eqs =
  match eqs with
    [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e] 



 
    
let mk_node name equations = { name=checkname name ; inputs=[]; outputs=[]; equations= mk_equations equations }
		     
let lustre_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    structure_item = fun mapper str ->
      match str.pstr_desc with
	Pstr_extension (({txt="node";_},PStr [s]),_) ->
	begin match s.pstr_desc with
		Pstr_value (_,[v]) ->
		let _signature = mk_node (v.pvb_pat) (v.pvb_expr) in
	        [%stri let () = () ]
	      | _ -> failwith "ok" 
	end
      | x -> default_mapper.structure_item mapper str
  }

let () = register "lustre" lustre_mapper 
