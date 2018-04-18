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

let get_num , reset =
  let cpt = ref 0 in
  (fun () -> incr cpt; !cpt) , (fun () -> cpt := 0 )


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
  | Array el -> l
  | Array_map (e,f) ->
     get_idents l e
  | Array_fold (e,f,e') ->
     let l = get_idents l e  in
     get_idents l e'
  | Imperative_update (e,_) -> get_idents l e
  | Array_get (e,e') ->
    let l = get_idents l e in
    get_idents l e'
  | Call e -> l
  | Application (i,_,e) ->
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

(* Extract name of a clock in ocaml attribute *)
let extract_clock attr =
  let (sl,p) = attr in
  match p with
  | PStr str ->
    begin
      match str with
        [x] ->
        begin
          match x.pstr_desc with
          | Pstr_eval (e,attr) ->
            begin
              match e.pexp_desc with
              | Pexp_ident {txt = (Lident v); loc} -> e
              | _ -> Error.syntax_error e.pexp_loc "the clock should only be an ident"
            end
          | _ -> Error.syntax_error x.pstr_loc "wrong form of clock"
        end
      | _ -> Error.syntax_error sl.loc "wrong clock"
    end
  | _ -> Error.syntax_error sl.loc "wrong attribute"

let get_int e =
  match e with
  | { pexp_desc = Pexp_constant c;
    pexp_loc ;
    pexp_attributes } ->
      begin match c with
        | Pconst_integer (i,s) -> int_of_string i
        | _ -> Error.syntax_error pexp_loc "not an integer"
      end
  | _ -> Error.syntax_error e.pexp_loc "not an integer"


(* transform expressions to node of the ocalustre AST *)
let make_expression e =
  let rec parse_updates e =
    match e with
    | [%expr [%e? e1] = [%e? e2] ] -> [(mk_expr e1,mk_expr e2)]
    | [%expr [%e? e1] = [%e? e2] ; [%e? e3] ] -> (mk_expr e1,mk_expr e2)::(parse_updates e3)
    | _ -> Error.print_error e.pexp_loc "Not an array update"
  and mk_expr e =
    (* List.iter ( fun (sl,p) ->  Format.fprintf Format.std_formatter "=>%s<=\n" sl.txt ) attr; *)
    let attr = e.pexp_attributes in
    let clk = if attr <> [] then
                let a = List.hd attr in
                let (sl,pl) = a in
                Some (sl,extract_clock a)
              else None
    in
  let exp = match e with
    | [%expr () ] -> { e_desc = Unit ; e_loc = e.pexp_loc }
    | { pexp_desc = Pexp_tuple el ; pexp_loc ; pexp_attributes} ->
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
      let v= mk_expr e1 in
      let nb = get_int e2 in
      let make_list_n v n =
        let rec loop n acc =
          if n = 0 then acc
          else loop (n-1) (v::acc)
        in
        loop n []
      in
      let l = make_list_n v nb in
      { e_desc = Array l ;
        e_loc = e.pexp_loc }
    | { pexp_desc = Pexp_array el;
        pexp_loc ;
        pexp_attributes } ->
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
        | _ -> assert false   (* only int/float /string ftm *)
      end
    | { pexp_desc = Pexp_construct ({ txt = (Lident s) ; loc} ,e);
        pexp_loc ;
        pexp_attributes } ->
      begin match e with
        | None -> { e_desc = Value (Enum s) ;
                    e_loc = pexp_loc }
        | _ ->  Error.syntax_error pexp_loc "Cannot be something else than an enumerated type"
      end

    | { pexp_desc = Pexp_constraint (e,t) ; pexp_loc; pexp_attributes } ->
      mk_expr e
    | {pexp_desc = Pexp_ident {txt = (Lident v); loc} ;
       pexp_loc ;
       pexp_attributes} -> { e_desc = Variable v ; e_loc = e.pexp_loc }
    | [%expr true] -> { e_desc = Value (Bool true) ; e_loc = e.pexp_loc }
    | [%expr false] -> { e_desc = Value (Bool false) ; e_loc = e.pexp_loc }
    | [%expr [%e? e1] fby [%e? e2] ]  ->
      { e_desc = Fby (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr [%e? e1] ->>> [%e? e2] ]  ->
      { e_desc = Fby (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr [%e? e1] ->> [%e? e2] ]  ->
      { e_desc = Fby (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr [%e? e1] >>> [%e? e2] ]  ->
      { e_desc = Fby (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr [%e? e1] --< [%e? e2] ]  ->
      { e_desc = Fby (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr [%e? e1] --> [%e? e2] ]  ->
      { e_desc = Arrow (mk_expr e1 , mk_expr e2);
        e_loc = e.pexp_loc  }
    | [%expr clock [%e? e1]] ->
      let clock = Clock (mk_expr e1) in
      { e_desc = clock; e_loc = e.pexp_loc }
    | [%expr eval ([%e? e1]) ] ->
      let app = Call (e1) in
      { e_desc = app ; e_loc = e.pexp_loc }
    | [%expr [%e? e1] [%e? e2] ] ->
      let app = Application(checkname_ident e1, get_num(), mk_expr e2) in
      { e_desc = app ; e_loc = e.pexp_loc }
    | [%expr [%e? e1] --@ not [%e? e2]] ->
      { e_desc = Whennot (mk_expr e1,mk_expr e2) ; e_loc = e.pexp_loc }
    | [%expr [%e? e1] --@ [%e? e2]] ->
      { e_desc = When (mk_expr e1,mk_expr e2) ; e_loc = e.pexp_loc }
    | _ ->
       let s =
         Format.asprintf "%a"
           Pprintast.expression e in
       Error.syntax_error e.pexp_loc s
  in
  match clk with
  |  Some (sl,clk) ->
    begin
      match sl.txt with
      | "when" -> { exp with e_desc = When (exp,mk_expr clk)}
      | "whenot" -> { exp with e_desc = Whennot (exp,mk_expr clk)}
      | "whennot" -> { exp with e_desc = Whennot (exp,mk_expr clk)}
      | "when_not" -> { exp with e_desc = Whennot (exp,mk_expr clk)}
      | _ -> Error.print_error clk.pexp_loc "Wrong attribute"
    end
  | None -> exp
  in
  mk_expr e

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
        | _ ->
           Error.syntax_error p.pexp_loc "this is not a type constraint"
      end
    in
    pat'
  | _ -> raise @@ Invalid_argument "pat_of_expr"

(* creates equation node in the AST *)
let mk_equation eq =
  match eq with
  | [%expr [%e? p] := [%e? e] ] | [%expr [%e? p] = [%e? e] ] ->
    begin
      match p.pexp_desc with
    | Pexp_ident _ ->
      {
        pattern= pat_of_pexp p;
        expression = make_expression e
    }
    | Pexp_tuple tu ->
     { pattern=  pat_of_pexp p;
       expression = make_expression e}
    | Pexp_constraint (e',t) ->

      { pattern = pat_of_pexp p ; expression = make_expression e}
    | _ ->
       Error.syntax_error eq.pexp_loc "in equation"
    end
      (*    | { pexp_desc = Pexp_apply (_, (p::e::_));
    pexp_loc ;
      pexp_attributes} ->
    print_endline "tuuuuple";
    {pattern= { p_desc =  Tuple (checkname_tuple p) ; p_loc = pexp_loc } ;
            expression = mk_expr (snd e)} *)
  | _ ->
     Error.syntax_error eq.pexp_loc "not an equation"



(* creates list of equations nodes in the AST *)
let rec mk_equations eqs =
  match eqs with
  | [%expr [%e? e1]; [%e? eq]] -> mk_equation e1 :: mk_equations eq
  | e -> [mk_equation e]

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


let rec parse_patt p =
  match p.ppat_desc with
      | Ppat_construct _ -> { p_desc = PUnit ; p_loc = p.ppat_loc }
      | Ppat_var s -> { p_desc = Ident s.txt ; p_loc = s.loc }
      | Ppat_tuple l -> { p_desc = Tuple (List.map (fun x -> parse_patt x) l) ; p_loc = p.ppat_loc }
     | Ppat_constraint (p,t) ->
        begin match t.ptyp_desc with
          | Ptyp_constr ({ loc ; txt = lid},_) ->
            { p_desc = Typed (parse_patt p , id_of_lid lid) ; p_loc = p.ppat_loc }
          | _ -> Error.syntax_error p.ppat_loc "this is not a type constraint"
        end
      | _ -> Error.syntax_error p.ppat_loc "unknown pattern format"

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
          | _ -> Error.syntax_error p.ppat_loc "this is not a type constraint"
        end
      | _ -> Error.syntax_error p.ppat_loc "unknown pattern format"
    else
      Error.syntax_error body.pexp_loc "wrong label"
  (*  | [%expr fun () -> [%e? body] ] -> ( [], body)
      | [%expr fun [%p? inputs] -> [%e? body] ] ->
      begin match inputs.ppat_desc with
        | Ppat_var s -> ([(checkname_pattern inputs)], body )
        | Ppat_tuple l -> (List.map (fun x -> checkname_pattern x) l, body) (* todo *)
        | _ -> (* Error.syntax_error body.pexp_loc *) failwith "okok"
      end *)
  | _ -> Error.syntax_error body.pexp_loc "wrong i/o"



(* creates a node "lustre node" in the AST *)
let mk_node name body =
  let name = checkname_pattern name in
  let inputs, body = checkio (Nolabel) body in
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
