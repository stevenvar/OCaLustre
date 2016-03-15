open Parsetree
open Asttypes
open Longident
(*
* AST 
*)

type clock = Base | On of clock * ident  
and stream = ident
and node = {
  name : ident; 
  inputs : stream list;  
  outputs : stream list; 
  equations : equation list; 
}
and equation = { 
  pattern : pattern ; 
  expression : expression; 
} 
and pattern = stream 
and constant = Integer of int
and expression = exp_desc * clock 
and ident = {
  loc : Location.t;
  content :  string
}
and exp_desc =   
  | Alternative of expression * expression * expression
  | InfixOp of inf_operator * expression * expression
  | PrefixOp of pre_operator * expression
  | Value of constant 
  | Variable of stream
  | Fby of constant * expression
  | When of expression * ident 
  | Unit
  (* merge ? *)
and inf_operator =
  | Diff
  | Equals
  | Plus
  | Minus
  | Times
  | Div
and pre_operator = 
  | Not

(*
* Errors 
*)

module Error = struct
  let print_error loc string =
    raise (Location.(Error(error ~loc:loc ("Error:"^string))))

  let syntax_error loc =
    print_error loc "Syntax Error in "
end 

(*
* AST makers 
*)

let loc_default = Location.none

let mk_pattern ?(loc=loc_default) v = {loc; content = v}
let mk_ident ?(loc=loc_default) v = { loc ; content = v } 

let alternative e1 e2 e3 = Alternative (e1, e2, e3)

let ( +/ ) e1 e2 = InfixOp ( Plus , e1 , e2 ) 
let ( */ ) e1 e2 = InfixOp ( Times , e1 , e2)
let ( -/ ) e1 e2 = InfixOp ( Minus, e1, e2)
let ( // ) e1 e2 = InfixOp (Div, e1, e2) 

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

(* Returns the idents inside each construct in a list *)
let rec get_idents (e,c) =
  match e with 
  | Variable i -> [i]
  | Alternative (e1,e2,e3) ->
    get_idents e1 @ 
    get_idents e2 @ 
    get_idents e3
  | InfixOp (op, e1, e2) ->
    get_idents e1 @
    get_idents e2
  | PrefixOp (op, e1) -> get_idents e1
  | Value v -> []
  | Unit -> []
  | Fby (i,e') -> get_idents e'
  | When (e',c) -> get_idents e' 
                 

(* transform expressions to node of the ocalustre AST *)
let rec mk_expr e =
  match e with
  | [%expr () ] -> Unit , Base 
  | [%expr [%e? e1] = [%e? e2] ] -> InfixOp(Equals, mk_expr e1, mk_expr e2) , Base 
  | [%expr [%e? e1] <> [%e? e2] ] -> InfixOp(Diff, mk_expr e1, mk_expr e2) , Base
  | [%expr [%e? e1] + [%e? e2] ] -> mk_expr e1 +/ mk_expr e2 , Base
  | [%expr [%e? e1] * [%e? e2] ] -> mk_expr e1 */ mk_expr e2 , Base 
  | [%expr [%e? e1] - [%e? e2] ] -> mk_expr e1 -/ mk_expr e2 , Base 
  | [%expr [%e? e1] / [%e? e2] ] -> mk_expr e1 // mk_expr e2 , Base 
  | [%expr if ([%e? e1]) then ([%e? e2]) else ([%e? e3]) ] ->
    alternative (mk_expr e1) (mk_expr e2) (mk_expr e3) , Base 
  | [%expr not [%e? e] ] -> mk_not (mk_expr e) , Base 
  | { pexp_desc = Pexp_constant c;
      pexp_loc ;
      pexp_attributes } ->
    begin match c with
      | Const_int i -> Value (Integer i ) , Base 
      | _ -> assert false   (* only int ftm *)
    end
  | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
     pexp_loc ;
     pexp_attributes} ->
    mk_variable v , Base 
  | [%expr [%e? e1] fby [%e? e2] ]  ->
    begin match e1 with
      | {pexp_desc = Pexp_constant c; pexp_loc ; pexp_attributes } ->
        begin match c with
          | Const_int i -> Fby (Integer i, mk_expr e2) , Base
          | _ -> failwith "error"
        end
      | _ -> failwith "syntax error"
    end
  | [%expr [%e? e1] on [%e? e2] ] -> let i = List.hd (get_idents (mk_expr e2)) in
    When ((mk_expr e1), i) , (On (Base, i))
  | _ ->
    Pprintast.expression Format.std_formatter e;
    Error.syntax_error e.pexp_loc 

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
  | [%expr [%e? p] = [%e? e] ] -> 
    {pattern= (checkname_ident p);
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
      | Ppat_var s -> ([(checkname_pattern inputs)], body ) 
      | Ppat_tuple l -> (List.map (fun x -> checkname_pattern x) l, body) (* todo *)
      | _ -> Error.syntax_error body.pexp_loc 
    end 
  | _ -> Error.syntax_error body.pexp_loc 



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

