open Parsetree
open Parsing_ast
open Asttypes
open Longident
open Error


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
let ( +./ ) e1 e2 = InfixOp ( Plusf , e1 , e2 )
let ( *./ ) e1 e2 = InfixOp ( Timesf , e1 , e2)
let ( -./ ) e1 e2 = InfixOp ( Minusf, e1, e2)
let ( /./ ) e1 e2 = InfixOp (Divf, e1, e2)
let ( =/ ) e1 e2 = InfixOp ( Equals , e1 , e2 )
let ( </ ) e1 e2 = InfixOp ( Inf , e1 , e2)
let ( <=/ ) e1 e2 = InfixOp ( Infe, e1, e2)
let ( >/ ) e1 e2 = InfixOp (Sup, e1, e2)
let ( >=/ ) e1 e2 = InfixOp (Supe, e1, e2)
let ( &&/ ) e1 e2 = InfixOp (Band, e1, e2)
let ( ||/ ) e1 e2 = InfixOp (Bor, e1, e2)

let mk_not e1 = PrefixOp ( Not , e1)

let string_of_pattern p =
  match p.ppat_desc with
  | Ppat_var sl -> sl.txt
  | _ -> failwith "not a good pattern"

(* check if the pattern is a variable *)
let rec checkname_pattern n =
  match n.ppat_desc with
  |  Ppat_var sl -> {p_loc=sl.loc ; p_desc= Ident sl.txt }
  |  Ppat_tuple t -> {p_loc=n.ppat_loc ; p_desc= Tuple (List.map checkname_pattern t) }
  | _ -> Error.print_error n.ppat_loc "this is not a pattern"

(* check if the name is an ident  *)
let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {loc; txt=Lident s } -> s
  | _ -> Error.print_error id.pexp_loc "this is not an ident"

(* check if the tuple is an ident list  *)
let checkname_tuple il =
  List.map checkname_ident il

(* Returns the idents inside each construct in a list *)
let rec get_idents l e =
  match e.e_desc with
  | Variable i -> i::l
  | Call e -> l
  | Application (i,e) ->
    get_idents l e
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
  | Pre e -> get_idents l e
  | Value v -> l
  | Unit -> l
  | Fby (i , e') ->
    let l = get_idents l i in
    get_idents l e'
  | Arrow (e1,e2) ->
    let l = get_idents l e1 in
    get_idents l e2
  | When (e',c) -> get_idents l e'
  | Whennot (e',c) -> get_idents l e'
  | ETuple (el) -> List.fold_left (fun accu e -> (get_idents l e)@accu) [] el
  | Merge (e1,e2,e3) ->
  let l = get_idents l e3 in
  let l = get_idents l e2 in
  let l = get_idents l e1 in
  l


(* transform expressions to node of the ocalustre AST *)
let rec mk_expr e =
  match e with
  | [%expr () ] -> { e_desc = Unit ; e_loc = e.pexp_loc }
  | { pexp_desc = Pexp_tuple el ; pexp_loc ; pexp_attributes} ->
    let l = List.map mk_expr el in
    { e_desc = ETuple (l) ;
      e_loc = pexp_loc }

  | [%expr [%e? e1] = [%e? e2] ] ->
    { e_desc = InfixOp(Equals, mk_expr e1, mk_expr e2) ;
      e_loc = e.pexp_loc }
  | [%expr [%e? e1] <> [%e? e2] ] ->
    { e_desc = InfixOp(Diff, mk_expr e1, mk_expr e2) ;
      e_loc = e.pexp_loc }
  | [%expr [%e? e1] + [%e? e2] ] -> { e_desc = mk_expr e1 +/ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] * [%e? e2] ] -> { e_desc = mk_expr e1 */ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] - [%e? e2] ] -> { e_desc = mk_expr e1 -/ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] / [%e? e2] ] -> { e_desc = mk_expr e1 // mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] +. [%e? e2] ] -> { e_desc = mk_expr e1 +./ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] *. [%e? e2] ] -> { e_desc = mk_expr e1 *./ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] -. [%e? e2] ] -> { e_desc = mk_expr e1 -./ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] /. [%e? e2] ] -> { e_desc = mk_expr e1 /./ mk_expr e2 ;
                                       e_loc = e.pexp_loc }
  | [%expr [%e? e1] > [%e? e2] ] -> { e_desc = mk_expr e1 >/ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] < [%e? e2] ] -> { e_desc = mk_expr e1 </ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] <= [%e? e2] ] -> { e_desc = mk_expr e1 <=/ mk_expr e2 ;
                                      e_loc = e.pexp_loc }
  | [%expr [%e? e1] >= [%e? e2] ] -> { e_desc = mk_expr e1 >=/ mk_expr e2 ;
                                       e_loc = e.pexp_loc }
  | [%expr [%e? e1] && [%e? e2] ] -> { e_desc = mk_expr e1 &&/ mk_expr e2 ;
                                       e_loc = e.pexp_loc }
  | [%expr [%e? e1] || [%e? e2] ] -> { e_desc = mk_expr e1 ||/ mk_expr e2 ;
                                       e_loc = e.pexp_loc }
  | [%expr [%e? e1] mod [%e? e2] ] -> { e_desc = InfixOp (Mod,mk_expr e1,mk_expr e2) ;
                                       e_loc = e.pexp_loc }
  | [%expr if ([%e? e1]) then ([%e? e2]) else ([%e? e3]) ] ->
    { e_desc = alternative (mk_expr e1) (mk_expr e2) (mk_expr e3) ;
      e_loc = e.pexp_loc }
  | [%expr merge ([%e? e1]) ([%e? e2]) ([%e? e3]) ] ->
    { e_desc = Merge ((mk_expr e1),(mk_expr e2),(mk_expr e3)) ;
      e_loc = e.pexp_loc }
  | [%expr not [%e? e] ] -> { e_desc = mk_not (mk_expr e) ;
                              e_loc = e.pexp_loc }
  | [%expr pre [%e? e] ] -> { e_desc = Pre (mk_expr e) ;
                              e_loc = e.pexp_loc }
  | [%expr ~- [%e? e] ] -> { e_desc = PrefixOp (Neg,(mk_expr e)) ;
                             e_loc = e.pexp_loc }
  | [%expr ~-. [%e? e] ] -> { e_desc = PrefixOp (Negf,(mk_expr e)) ;
                              e_loc = e.pexp_loc }
  | { pexp_desc = Pexp_constant c;
      pexp_loc ;
      pexp_attributes } ->
    begin match c with
      | Pconst_integer (i,s) -> { e_desc = Value (Integer (int_of_string i)) ;
                                  e_loc = e.pexp_loc }
      | Pconst_float (f,s) -> { e_desc = Value (Float (float_of_string f)) ;
                                e_loc = e.pexp_loc }
      | Pconst_string (str,s) -> { e_desc = Value (String str);
                                   e_loc = e.pexp_loc }
      | _ -> assert false   (* only int/float ftm *)
    end
  | { pexp_desc = Pexp_construct ({ txt = (Lident s) ; loc} ,e);
      pexp_loc ;
      pexp_attributes } ->
     begin match e with
      | None -> { e_desc = Value (Enum s) ;
                                e_loc = pexp_loc }
      | _ -> assert false   (* only enum types *)
    end

  | { pexp_desc = Pexp_constraint (e,t) ; pexp_loc; pexp_attributes } ->
    mk_expr e
  | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
     pexp_loc ;
     pexp_attributes} -> { e_desc = Variable v ; e_loc = e.pexp_loc }
  | [%expr true] -> { e_desc = Value (Bool true) ; e_loc = e.pexp_loc }
  | [%expr false] -> { e_desc = Value (Bool false) ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] ->> [%e? e2] ]  ->
    { e_desc = Fby (mk_expr e1 , mk_expr e2);
      e_loc = e.pexp_loc  }
  | [%expr [%e? e1] --> [%e? e2] ]  ->
    { e_desc = Arrow (mk_expr e1 , mk_expr e2);
      e_loc = e.pexp_loc  }
  | [%expr [%e? e1] @whn [%e? e2] ] ->
    { e_desc =  When (mk_expr e1 , mk_expr e2) ;
      e_loc = e.pexp_loc }
  | [%expr [%e? e1] @whnot [%e? e2] ] ->
    { e_desc =  Whennot (mk_expr e1 , mk_expr e2) ;
      e_loc = e.pexp_loc }
  | [%expr call ([%e? e1]) ] ->
    let app = Call (e1)
    in
    { e_desc = app ; e_loc = e.pexp_loc }
  | [%expr [%e? e1] [%e? e2] ] ->
    let app = Application(checkname_ident e1, mk_expr e2)
    in
    { e_desc = app ; e_loc = e.pexp_loc }

  | _ ->
    Pprintast.expression Format.std_formatter e;
    Error.syntax_error e.pexp_loc

let id_of_lid lid =
  match lid with
  | Lident li -> li
  | _ -> raise @@ Invalid_argument "id_of_lid"

let rec pat_of_pexp p =
  match p.pexp_desc with
  | Pexp_ident i -> { p_desc = Ident (id_of_lid i.txt) ;
                  p_loc = p.pexp_loc ; }
  | Pexp_tuple t ->
    let tl = List.map (fun p -> pat_of_pexp p) t in
    { p_desc = Tuple tl ;
      p_loc = p.pexp_loc ;
    }
   | Pexp_constraint (e',t) ->
      let pat' =
        begin match t.ptyp_desc with
          | Ptyp_constr ({ loc ; txt = lid},_) ->
            { p_desc = Typed (pat_of_pexp e' , id_of_lid lid) ; p_loc = p.pexp_loc }
          | _ -> Error.syntax_error p.pexp_loc
        end
      in
      pat'
  | _ -> raise @@ Invalid_argument "pat_of_expr"

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
  | [%expr [%e? p] = [%e? e] ] ->
    begin
      match p.pexp_desc with
    | Pexp_ident _ ->
      {
        pattern= pat_of_pexp p;
        expression = mk_expr e
    }
    | Pexp_tuple tu ->
     { pattern=  pat_of_pexp p;
       expression = mk_expr e}
    | Pexp_constraint (e',t) ->

      { pattern = pat_of_pexp p ; expression = mk_expr e}
    | _ -> Error.syntax_error eq.pexp_loc
    end
      (*    | { pexp_desc = Pexp_apply (_, (p::e::_));
    pexp_loc ;
      pexp_attributes} ->
    print_endline "tuuuuple";
    {pattern= { p_desc =  Tuple (checkname_tuple p) ; p_loc = pexp_loc } ;
            expression = mk_expr (snd e)} *)
  | _ -> Error.syntax_error eq.pexp_loc



(* creates list of equations nodes in the AST *)
let rec mk_equations eqs =
  match eqs with
  | [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e]

let mk_pre b =
  match b with
  | [%expr pre [%e? e] ; [%e? e' ] ] -> Some (mk_expr e) , e'
  | _ -> None , b

let mk_post b =
  match b with
  | [%expr post [%e? e] ; [%e? e' ] ] -> Some (mk_expr e) , e'
  | _ -> None , b

let mk_inv b =
  match b with
  | [%expr inv [%e? e] ; [%e? e' ] ] -> Some (mk_expr e) , e'
  | _ -> None , b


let rec parse_patt p =
  match p.ppat_desc with
      | Ppat_construct _ -> { p_desc = PUnit ; p_loc = p.ppat_loc }
      | Ppat_var s -> { p_desc = Ident s.txt ; p_loc = s.loc }
      | Ppat_tuple l -> { p_desc = Tuple (List.map (fun x -> parse_patt x) l) ; p_loc = p.ppat_loc }
     | Ppat_constraint (p,t) ->
        begin match t.ptyp_desc with
          | Ptyp_constr ({ loc ; txt = lid},_) ->
            { p_desc = Typed (parse_patt p , id_of_lid lid) ; p_loc = p.ppat_loc }
          | _ -> Error.syntax_error p.ppat_loc
        end
      | _ -> Error.syntax_error p.ppat_loc

(* check that the I/O are tuples and returns a tuple of corresponding idents *)
let checkio s ({pexp_desc; pexp_loc; pexp_attributes} as body) =
  match pexp_desc with
  | Pexp_fun (l,_,p,e) ->
    if s = l then
      match p.ppat_desc with
      | Ppat_construct _ -> { p_desc = PUnit ; p_loc = p.ppat_loc } , e
      | Ppat_var s -> { p_desc = Ident s.txt ; p_loc = s.loc }, e
      | Ppat_tuple l -> parse_patt p, e
      | Ppat_constraint (p,t) ->
        begin match t.ptyp_desc with
          | Ptyp_constr ({ loc ; txt = lid},_) ->
            { p_desc = Typed (parse_patt p , id_of_lid lid) ; p_loc = p.ppat_loc } , e
          | _ -> Error.syntax_error p.ppat_loc
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
  let pre,body = mk_pre body in
  let post,body = mk_post body in
  let inv,body = mk_inv body in
  let equations = mk_equations body in
  {
    pre;
    post;
    inv;
    name;
    inputs;
    outputs;
    equations
  }
