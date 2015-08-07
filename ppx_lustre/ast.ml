open Parsetree
open Asttypes
open Longident
(*
* AST TYPES 
*)

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
 pattern = ident 
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
  | Application of ident * expression list 
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

(*
* Errors 
*)

module Error = struct
  let print_error loc string =
    raise (Location.(Error(error ~loc:loc ("Error:"^string))))

  let syntax_error loc =
    print_error loc "Syntax Error"
end 

(*
* AST makers 
*)

let loc_default = Location.none
					      
let mk_ident ?(loc=loc_default) v = { loc ; content = v } 

let alternative e1 e2 e3 = Alternative (e1, e2, e3)

let (+) e1 e2 = InfixOp ( Plus , e1 , e2 ) 

let ( * ) e1 e2 = InfixOp ( Times , e1 , e2)

let ( - ) e1 e2 = InfixOp ( Minus, e1, e2)

let ( / ) e1 e2 = InfixOp (Div, e1, e2) 

let (-->) e1 e2 = InfixOp ( Arrow, e1, e2) 

let mk_pre e1 = PrefixOp ( Pre , e1) 

let mk_not e1 = PrefixOp ( Not , e1) 
  
let mk_variable v  = Variable (mk_ident v)

(* check if the pattern is a variable *)
let checkname_pattern n =
  match n.ppat_desc with
    Ppat_var sl -> {loc=sl.loc ; content=sl.txt }
  | _ -> Error.print_error n.ppat_loc "this is not a pattern"

(* check if the name is an ident  *)
let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> mk_ident ~loc s
  | _ -> Error.print_error id.pexp_loc "this is not an expression" 

(* transform expressions to node of the ocalustre AST *)
let rec mk_expr e =
  match e with
  | [%expr [%e? e1] + [%e? e2] ] -> mk_expr e1 + mk_expr e2
  | [%expr [%e? e1] * [%e? e2] ] -> mk_expr e1 * mk_expr e2
  | [%expr [%e? e1] - [%e? e2] ] -> mk_expr e1 - mk_expr e2
  | [%expr [%e? e1] / [%e? e2] ] -> mk_expr e1 / mk_expr e2
  | [%expr pre [%e? e1] ] -> mk_pre (mk_expr e1)
  | [%expr [%e? e1] next [%e? e2] ] -> (mk_expr e1) --> (mk_expr e2) 
  | [%expr if [%e? e1] then [%e? e2] else [%e? e3] ] ->
    alternative (mk_expr e1) (mk_expr e2) (mk_expr e3)
  | [%expr true ] -> Value e
  | [%expr false ] -> Value e
  (* a := NOEUD2 (x,y) *)
  | [%expr [%e? e1] [%e? e2] ] -> Application(checkname_ident e1, 
    begin match e2.pexp_desc with
      | Pexp_tuple l -> List.map mk_expr l
      | _ -> [mk_expr e2]
    end )
  | [%expr not [%e? e1] ] -> mk_not (mk_expr e1)
  | { pexp_desc = Pexp_constant c;
      pexp_loc ;
      pexp_attributes } ->
    Value (e)
  | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
     pexp_loc ;
     pexp_attributes} ->
    mk_variable v 
  | _ -> Error.syntax_error e.pexp_loc 

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
    [%expr [%e? p] := [%e? e] ] ->
    {pattern= checkname_ident p;
     expression = mk_expr e}
  | _ -> Error.syntax_error eq.pexp_loc 

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
      | Ppat_var s -> ([checkname_pattern inputs], body ) 
      | Ppat_tuple l -> (List.map checkname_pattern l, body)
      | _ -> Error.syntax_error body.pexp_loc 
    end 
  | _ -> Error.syntax_error body.pexp_loc 

let rec get_idents e =
  match e with 
  | Variable i -> [i] 
  | Alternative (e1,e2,e3) ->
    get_idents e1 @ 
    get_idents e2 @
    get_idents e3
  | Application (id, el) ->
    List.fold_left (fun l e -> l @ get_idents e) [] el 
  | InfixOp (op, e1, e2) ->
      get_idents e1 @
      get_idents e2
  | PrefixOp (op, e1) ->
    begin match op with
      | Pre -> []
      | _ -> get_idents e1
    end
  | Value v -> []

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

