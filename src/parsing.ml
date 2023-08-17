open Parsetree
open Parsing_ast
open Asttypes
open Longident

(** some useful functions **)

let loc_default = Location.none

let mk_pattern ?(loc=loc_default) v = { p_desc = (Ident v) ; p_loc = loc }

let get_num , reset =
  let cpt = ref 0 in
  (fun () -> incr cpt; !cpt) , (fun () -> cpt := 0 )

(** Functions that create AST chunks **)

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

(** check if the pattern is a variable **)
let rec checkname_pattern n =
  match n.ppat_desc with
  | Ppat_var sl -> {p_loc=sl.loc ; p_desc= Ident sl.txt }
  | Ppat_tuple t -> {p_loc=n.ppat_loc ; p_desc= Tuple (List.map checkname_pattern t) }
  | _ -> Error.print_error n.ppat_loc "Not a pattern"

(** check if the name is an ident **)
let checkname_ident id =
  match id.pexp_desc with
    Pexp_ident {txt=Lident s ; _} -> s
  | _ -> Error.print_error id.pexp_loc "Not an ident"

(** check if the tuple is an ident list **)
let checkname_tuple il =
  List.map checkname_ident il

(** Returns the idents inside each construct **)
let rec get_idents l e =
  match e.e_desc with
  | Variable i -> i::l
  | Array _ -> l
  | Array_map (e,_) -> get_idents l e
  | Array_fold (e,_,e') ->
    let l = get_idents l e in
    get_idents l e'
  | Imperative_update (e,_) -> get_idents l e
  | Array_get (e,e') ->
    let l = get_idents l e in
    get_idents l e'
  | Call (_,e) ->
    get_idents l e
  | Application (_,_,e) ->
    get_idents l e
  | Alternative (e1,e2,e3) ->
    let l = get_idents l e3 in
    let l = get_idents l e2 in
    get_idents l e1
  | InfixOp (_, e1, e2) ->
    let l = get_idents l e2 in
    get_idents l e1
  | PrefixOp (_, e1) -> get_idents l e1
  | Pre e -> get_idents l e
  | Value _ -> l
  | Unit -> l
  | Fby (i , e') ->
    let l = get_idents l i in
    get_idents l e'
  | Arrow (e1,e2) ->
    let l = get_idents l e1 in
    get_idents l e2
  | When (e',_) -> get_idents l e'
  | Whennot (e',_) -> get_idents l e'
  | ETuple (el) ->
    List.concat_map (get_idents l) el
  | Clock e -> get_idents l e
  | Merge (e1,e2,e3) ->
    let l = get_idents l e3 in
    let l = get_idents l e2 in
    get_idents l e1

(** Extract name of a clock in an OCaml attribute **)
let extract_clock { attr_name = s; attr_payload = p; _} =
  let extract_ident b e =
    match e.pexp_desc with
    | Pexp_ident _ -> (b,Some e)
    | _ -> Error.syntax_error e.pexp_loc "The clock should only be an ident"
  in
  let b = match s.txt with
    | "when" -> true
    | "whennot" -> false
    | _ -> Error.syntax_error s.loc "Wrong attribute: expecting when or whennot"
  in
  match p with
  | PStr [{pstr_desc = Pstr_eval (e,_); _ }] ->
    extract_ident b e
  | PStr _ -> (false,None)
  | _ -> Error.syntax_error s.loc "Wrong attribute: expecting a structure"


(** Extract the int in a Pexp_constant **)
let get_int e =
  match e with
  | { pexp_desc = Pexp_constant c;
      pexp_loc ; _ } ->
    begin match c with
      | Pconst_integer (i,_) -> int_of_string i
      | _ -> Error.syntax_error pexp_loc "not an integer"
    end
  | _ -> Error.syntax_error e.pexp_loc "not a constant expression"

(** Transforms OCaml expressions to node of the OCaLustre AST **)

let make_expression e =
  let rec parse_updates e =
    match e with
    | [%expr [%e? e1] = [%e? e2] ] -> [(mk_expr e1,mk_expr e2)]
    | [%expr [%e? e1] = [%e? e2] ; [%e? e3] ] -> (mk_expr e1,mk_expr e2)::(parse_updates e3)
    | _ -> Error.print_error e.pexp_loc "Not an array update"
  and mk_expr e =
    let attr = e.pexp_attributes in
    let clocks = List.map extract_clock attr in
    let exp = match e with
      | [%expr () ] -> { e_desc = Unit ; e_loc = e.pexp_loc }
      | { pexp_desc = Pexp_tuple el ; pexp_loc ; _} ->
        let l = List.map mk_expr el in
        { e_desc = ETuple l ;
          e_loc = pexp_loc }
      | [%expr [%e? e1].fold([%e? f],[%e? e']) ] ->
        { e_desc = Array_fold (mk_expr e1, f, mk_expr e');
          e_loc = e.pexp_loc }
      | [%expr [%e? e1].map([%e? e2]) ] ->
        { e_desc = Array_map (mk_expr e1, e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1].([%e? e2]) ] ->
        { e_desc = Array_get (mk_expr e1, mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1].update([%e? e2])  ] ->
        { e_desc = Imperative_update (mk_expr e1, parse_updates e2);
          e_loc = e.pexp_loc}
      | [%expr [%e? e1] where [%e? e2]  ] ->
        { e_desc = Imperative_update (mk_expr e1, parse_updates e2);
          e_loc = e.pexp_loc}
      | [%expr [| [%e? e1] ^ [%e? e2] |] ] ->
        let v = mk_expr e1 in
        let nb = get_int e2 in
        let l = List.init nb (fun _ -> v) in
        { e_desc = Array l ;
          e_loc = e.pexp_loc }
      | { pexp_desc = Pexp_array el;
          pexp_loc ; _ } ->
        let l = List.map mk_expr el in
        { e_desc = Array l ;
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
      | [%expr [%e? e1] mod [%e? e2] ] ->
        { e_desc = InfixOp (Mod,mk_expr e1,mk_expr e2) ;
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
      | [%expr true] -> { e_desc = Value (Bool true) ; e_loc = e.pexp_loc }
      | [%expr false] -> { e_desc = Value (Bool false) ; e_loc = e.pexp_loc }
      | { pexp_desc = Pexp_constant c; pexp_loc; _ } ->
        begin match c with
          | Pconst_integer (i,None) -> { e_desc = Value (Integer (int_of_string i));
                                         e_loc = pexp_loc }
          | Pconst_float (f,None) -> { e_desc = Value (Float (float_of_string f));
                                       e_loc = pexp_loc }
          | Pconst_char c -> { e_desc = Value (Char c);
                               e_loc = pexp_loc }
          | Pconst_string (str,loc,None) -> { e_desc = Value (String str);
                                              e_loc = loc }
          | _ -> Error.syntax_error e.pexp_loc "Wrong constant value"
        end
      | { pexp_desc = Pexp_construct ({ txt = (Lident s) ; _}, None); pexp_loc ; _ } ->
        { e_desc = Value (Enum s) ; e_loc = pexp_loc }
      | { pexp_desc = Pexp_construct _; pexp_loc ; _ } ->
        Error.syntax_error pexp_loc
          "A sum type cannot be something else than an enumerated type"
      | { pexp_desc = Pexp_constraint (e,_); _ } -> mk_expr e
      | {pexp_desc = Pexp_ident {txt = (Lident v); _} ; _ } ->
        { e_desc = Variable v ; e_loc = e.pexp_loc }
      | [%expr [%e? e1] fby [%e? e2]]  ->
        { e_desc = Fby (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] ->>> [%e? e2]]  ->
        { e_desc = Fby (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] ->> [%e? e2]]  ->
        { e_desc = Fby (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] >>> [%e? e2]]  ->
        { e_desc = Fby (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] --< [%e? e2]]  ->
        { e_desc = Fby (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] => [%e? e2]]  ->
        { e_desc = Arrow (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr [%e? e1] --> [%e? e2]]  ->
        { e_desc = Arrow (mk_expr e1 , mk_expr e2);
          e_loc = e.pexp_loc }
      | [%expr clock [%e? e1]] ->
        let clock = Clock (mk_expr e1) in
        { e_desc = clock; e_loc = e.pexp_loc }
      | [%expr [%e? e1] [%e? e2] ] ->
        let app = Application(checkname_ident e1, get_num(), mk_expr e2) in
        { e_desc = app ; e_loc = e.pexp_loc }
      | [%expr [%e? e1] @ not [%e? e2]] ->
        { e_desc = Whennot (mk_expr e1,mk_expr e2) ; e_loc = e.pexp_loc }
      | [%expr [%e? e1] @ [%e? e2]] ->
        { e_desc = When (mk_expr e1,mk_expr e2) ; e_loc = e.pexp_loc }
      |  { pexp_desc = Pexp_apply ([%expr call],params); pexp_loc ; _ } ->
        begin match List.map snd params with
          | n::arg ->
            let exp = {e_desc = ETuple (List.map mk_expr arg); e_loc = e.pexp_loc} in
            { e_desc = Call(checkname_ident n, exp);
              e_loc = pexp_loc }
          | _ -> Error.syntax_error e.pexp_loc "Not a call"
        end
      | _ ->
        let s =
          Format.asprintf "%a" Pprintast.expression e in
        Error.syntax_error e.pexp_loc s
    in
    let create_sampled e cl =
      let rec aux l acc =
        match l with
        | [] -> acc
        | (true, Some s)::t -> aux t ({ e_desc = When (acc,mk_expr s) ; e_loc = e.e_loc})
        | (false, Some s)::t -> aux t ({ e_desc = Whennot (acc,mk_expr s) ; e_loc = e.e_loc})
        | (_, None)::t -> aux t acc
      in
      aux cl e
    in
    create_sampled exp clocks
  in
  mk_expr e

let id_of_lid lid =
  match lid with
  | Lident li -> li
  | _ -> raise @@ Invalid_argument "id_of_lid"

let rec pat_of_pexp p =
  match p.pexp_desc with
  | Pexp_ident i -> { p_desc = Ident (id_of_lid i.txt) ;
                      p_loc = p.pexp_loc }
  | Pexp_tuple t ->
    let tl = List.map (fun p -> pat_of_pexp p) t in
    { p_desc = Tuple tl ; p_loc = p.pexp_loc }
  | Pexp_constraint (e',{ptyp_desc = Ptyp_constr (lidloc,_) ; _}) ->
    { p_desc = Typed (pat_of_pexp e', id_of_lid lidloc.txt) ; p_loc = p.pexp_loc }
  | _ -> raise @@ Invalid_argument "pat_of_expr"

(** Creates equation in the AST **)
let mk_equation eq =
  match eq with
  | [%expr [%e? p] := [%e? e] ] | [%expr [%e? p] = [%e? e] ] ->
    begin
      match p.pexp_desc with
      | Pexp_ident _ ->
        { pattern = pat_of_pexp p; expression = make_expression e}
      | Pexp_tuple _ ->
        { pattern = pat_of_pexp p; expression = make_expression e}
      | Pexp_constraint _ ->
        { pattern = pat_of_pexp p; expression = make_expression e}
      | _ ->
        Error.syntax_error eq.pexp_loc
          "Equation in wrong form (maybe you need to add parentheses)"
    end
  | _ -> Error.syntax_error eq.pexp_loc "Not an equation"



(** Creates list of equations nodes in the AST **)
let rec mk_equations eqs =
  match eqs with
  | [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e]


(** Contracts **)

let mk_pre b =
  match b with
  | [%expr pre [%e? e] ; [%e? e' ] ] -> Some (make_expression e) , e'
  | _ -> None , b

let mk_post b =
  match b with
  | [%expr post [%e? e] ; [%e? e' ] ] -> Some (make_expression e) , e'
  | _ -> None , b

let mk_inv b =
  match b with
  | [%expr inv [%e? e] ; [%e? e' ] ] -> Some (make_expression e) , e'
  | _ -> None , b

(** Parse a pattern **)
let rec parse_patt p =
  match p.ppat_desc with
  | Ppat_construct _ -> { p_desc = PUnit; p_loc = p.ppat_loc }
  | Ppat_var s -> { p_desc = Ident s.txt; p_loc = s.loc }
  | Ppat_tuple l -> { p_desc = Tuple (List.map (fun x -> parse_patt x) l);
                      p_loc = p.ppat_loc }
  | Ppat_constraint (p,{ptyp_desc; _}) ->
    begin match ptyp_desc with
      | Ptyp_constr (idloc,_) ->
        { p_desc = Typed (parse_patt p , id_of_lid idloc.txt) ;
          p_loc = p.ppat_loc }
      | _ -> Error.syntax_error p.ppat_loc "Not a type constraint"
    end
  | _ -> Error.syntax_error p.ppat_loc "Unknown pattern format"

(** Check that the I/O are tuples and returns a tuple of corresponding idents **)
let checkio s body =
  match body.pexp_desc with
  | Pexp_fun (l,_,p,e) when l=s -> parse_patt p, e
  | Pexp_fun _ -> Error.syntax_error body.pexp_loc "Wrong label"
  | _ -> Error.syntax_error body.pexp_loc "Wrong I/O"

(** creates an OCaLustre node **)
let mk_node name body =
  let name = checkname_pattern name in
  let inputs, body = checkio Nolabel body in
  let outputs, body = checkio (Labelled "return") body in
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
