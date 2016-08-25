open Parsetree
open Parsing_ast
open Asttypes
open Longident

(*
* Errors
*)

module Error = struct
  let print_error loc string =
    raise (Location.Error (Location.error ~loc:loc ("Error:"^string)))

  let syntax_error loc =
    print_error loc "Syntax Error"
end

(*
* AST makers
*)

let loc_default = Location.none

let mk_pattern ?(loc=loc_default) v = { p_desc = (Ident v) ; p_loc = loc }

let alternative e1 e2 e3 = Alternative (e1, e2, e3)

let ( +/ ) e1 e2 = InfixOp ( Plus , e1 , e2 )
let ( */ ) e1 e2 = InfixOp ( Times , e1 , e2)
let ( -/ ) e1 e2 = InfixOp ( Minus, e1, e2)
let ( // ) e1 e2 = InfixOp (Div, e1, e2)

let mk_not e1 = PrefixOp ( Not , e1)


(* check if the pattern is a variable *)
let checkname_pattern n =
  match n.ppat_desc with
    Ppat_var sl -> {p_loc=sl.loc ; p_desc= Ident sl.txt }
  | _ -> Error.print_error n.ppat_loc "this is not a pattern"

(* check if the name is an ident  *)
let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> s
  | _ -> Error.print_error id.pexp_loc "this is not an expression"

(* Returns the idents inside each construct in a list *)
let rec get_idents l e =
  match e.e_desc with
  | Variable i -> i::l
  | Application (i,el) ->
     List.fold_left (fun accu e -> (get_idents l e)@accu) [] el
  | Alternative (e1,e2,e3) ->
    let l = get_idents l e3 in
    let l = get_idents l e2 in
    let l = get_idents l e1 in
    l
  | InfixOp (op, e1, e2) ->
    let l = get_idents l e2 in
    let l = get_idents l e1 in
    l
  | PrefixOp (op, e1) -> get_idents l e1
  | Value v -> l
  | Unit -> l
  | Fby (i , e') -> get_idents l e'
  | Arrow (i, e') -> get_idents l e'
  | When (e',c) -> get_idents l e'
  | Pre (e) ->  get_idents l e


(* transform expressions to node of the ocalustre AST *)
let rec mk_expr e =
  match e with
  | [%expr () ] -> { e_desc = Unit ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] = [%e? e2] ] -> { e_desc = InfixOp(Equals, mk_expr e1, mk_expr e2) ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] <> [%e? e2] ] -> { e_desc = InfixOp(Diff, mk_expr e1, mk_expr e2) ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] + [%e? e2] ] -> { e_desc = mk_expr e1 +/ mk_expr e2 ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] * [%e? e2] ] -> { e_desc = mk_expr e1 */ mk_expr e2 ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] - [%e? e2] ] -> { e_desc = mk_expr e1 -/ mk_expr e2 ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] / [%e? e2] ] -> { e_desc = mk_expr e1 // mk_expr e2 ; e_loc = e.pexp_loc }
  | [%expr if ([%e? e1]) then ([%e? e2]) else ([%e? e3]) ] ->
    { e_desc = alternative (mk_expr e1) (mk_expr e2) (mk_expr e3) ; e_loc = e.pexp_loc }
  | [%expr not [%e? e] ] -> { e_desc = mk_not (mk_expr e) ; e_loc = e.pexp_loc }
  | { pexp_desc = Pexp_constant c;
      pexp_loc ;
      pexp_attributes } ->
    begin match c with
      | Pconst_integer (i,s) -> { e_desc = Value (Integer (int_of_string i)) ; e_loc = e.pexp_loc }
      | Pconst_float (f,s) -> { e_desc = Value (Float (float_of_string f)) ; e_loc = e.pexp_loc }
      | _ -> assert false   (* only int/float ftm *)
    end
  | { pexp_desc = Pexp_constraint (e,t) ; pexp_loc; pexp_attributes } ->
    failwith "constraint"
  | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
     pexp_loc ;
     pexp_attributes} -> { e_desc = Variable v ; e_loc = e.pexp_loc }
  | [%expr true] -> { e_desc = Value (Bool true) ; e_loc = e.pexp_loc }
  | [%expr false] -> { e_desc = Value (Bool false) ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] ->> [%e? e2] ]  -> { e_desc = Fby (mk_expr e1 , mk_expr e2) ; e_loc = e.pexp_loc  }
  | [%expr [%e? e1] --> [%e? e2] ]  ->  { e_desc = Arrow (mk_expr e1 , mk_expr e2) ; e_loc = e.pexp_loc  }
  | [%expr [%e? e1] @> [%e? e2] ] -> { e_desc =  When (mk_expr e1 , mk_expr e2) ; e_loc = e.pexp_loc }
  | [%expr pre [%e? e1]] -> { e_desc = Pre (mk_expr e1) ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] [%e? e2] ] ->
     let app = Application(checkname_ident e1,
                 begin match e2.pexp_desc with
                 | Pexp_tuple l -> List.map mk_expr l
                 | _ -> [mk_expr e2]
                 end )
     in
     { e_desc = app ; e_loc = e.pexp_loc }
  | _ ->
    Pprintast.expression Format.std_formatter e;
    Error.syntax_error e.pexp_loc

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
  | [%expr [%e? p] = [%e? e] ] ->
    {pattern= { p_desc =  Ident (checkname_ident p) ; p_loc = p.pexp_loc } ;
     expression = mk_expr e}
  | _ -> Error.syntax_error eq.pexp_loc



(* creates list of equations nodes in the AST *)
let rec mk_equations eqs =
  match eqs with
  | [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e]

(* check that the I/O are tuples and returns a list of corresponding idents *)
let checkio s ({pexp_desc; pexp_loc; pexp_attributes} as body) =

  match pexp_desc with
  | Pexp_fun (l,_,p,e) ->
    if s = l then
      match p.ppat_desc with
      | Ppat_construct _ -> [], e
      | Ppat_var s -> [checkname_pattern p], e
      | Ppat_tuple l -> List.map (fun x -> checkname_pattern x) l, e
      | Ppat_constraint (p,t) ->
        begin match p.ppat_desc with
          | Ppat_var s -> [checkname_pattern p], e
          | _ -> failwith "unknown"
        end
      | _ -> Error.syntax_error p.ppat_loc
    else
      Error.syntax_error body.pexp_loc
(*  | [%expr fun () -> [%e? body] ] -> ( [], body)
  | [%expr fun [%p? inputs] -> [%e? body] ] ->
    begin match inputs.ppat_desc with
      | Ppat_var s -> ([(checkname_pattern inputs)], body )
      | Ppat_tuple l -> (List.map (fun x -> checkname_pattern x) l, body) (* todo *)
      | _ -> (* Error.syntax_error body.pexp_loc *) failwith "okok"
    end *)
  | _ -> Error.syntax_error body.pexp_loc



(* creates a node "lustre node" in the AST *)
let mk_node name body =
  let name = checkname_pattern name in
  let inputs, body = checkio (Labelled "i") body in
  let outputs, body = checkio (Labelled "o") body in
  let equations = mk_equations body in
  {
    name;
    inputs;
    outputs;
    equations
  }
