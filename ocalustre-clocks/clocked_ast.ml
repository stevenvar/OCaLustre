open Ast
open Ast_printer


type clock = Base | On of clock * Ast.ident  
and cnode = {
  cname : ident;
  cinputs : cstream list;
  coutputs : cstream list;
  cequations : cequation list; }
and cstream = stream * clock
and cequation = {
  cpattern : cpattern;
  cexpression : cexpression}
and cexpression = cexp_desc * clock 
and cpattern = pattern * clock
and cident = ident * clock
and cexp_desc =
  | CAlternative of cexpression * cexpression * cexpression
  | CInfixOp of inf_operator * cexpression * cexpression
  | CPrefixOp of pre_operator * cexpression
  | CValue of constant 
  | CVariable of cstream
  | CFby of constant * cexpression
  | CWhen of cexpression * cident 
  | CUnit
  | CCurrent of cexpression

let rec get_clock hst e =
  match e with 
  | Variable i ->
    (try
      Hashtbl.find hst i.content
    with _ -> Base) 
  | Alternative (e1, e2, e3) -> get_clock hst e2
  | InfixOp (op, e1, e2) -> get_clock hst e1
  | PrefixOp (op, e) -> get_clock hst e
  | Value v -> Base
  | Fby (v,e) -> get_clock hst e
  | When (e, i) -> On ((get_clock hst e), i) (* the clock is in the expression *)
  | Unit -> Base
  | Current e ->
    match get_clock hst e with
    | Base -> Base
    | On (c,i) -> c 

let rec clock_err cl =
  match cl with
  | Base::t -> clock_err t
  | On (c,i)::_ -> Error.print_error i.loc "Incoherent clock" 
  | [] -> failwith "no clocks"

let rec clock_eq c1 c2 =
  match c1,c2 with
  | Base, Base -> true
  | On (c1,i), On(c2,j) -> (clock_eq c1 c2) && (i.content = j.content) 
  | _ -> false  

let rec clock_exp hst e =
  match e with
  | Variable i ->
    let ck = try Hashtbl.find hst i.content with _ -> Base in 
    (CVariable (i, ck), ck)
  | Alternative (e1,e2,e3) ->
    let (e1,c1) as ce1 = clock_exp hst e1 in
    let (e2,c2) as ce2 = clock_exp hst e2 in
    let (e3,c3) as ce3 = clock_exp hst e3 in
    let e = CAlternative (ce1, ce2, ce3) in 
    if (clock_eq c1 c2 && clock_eq c2 c3) then e,c1
    else
      clock_err [c1;c2;c3]
  | InfixOp (op, e1, e2) ->
    let (e1, c1) as ce1 = clock_exp hst e1 in
    let (e2, c2) as ce2 = clock_exp hst e2 in 
    let e = CInfixOp (op, ce1, ce2) in
    if clock_eq c1 c2  then e, c1
    else
      clock_err [c1;c2]
  | PrefixOp (op, e') ->
    let (e,c) as ce = clock_exp hst e' in
    CPrefixOp (op, ce), c
  | Value v -> CValue v, Base 
  | Fby (v,e') ->
    let (e,c) as ce = clock_exp hst e' in
    CFby (v, ce), c
  | When (e', i) ->
    let (e,c) as ce = clock_exp hst e' in
    let ck = On (c,i) in
    CWhen (ce,(i,ck)), ck 
  | Unit -> CUnit, Base
  | Current e' ->
    let (e,c) as ce = clock_exp hst e' in
    let ck =  match get_clock hst e' with
    | Base -> Base
    | On (c,i) -> c in
    CCurrent ce, ck

let clock_equations hst el =
  let clock_eq e =
    let (e',ck) as ce = clock_exp hst e.expression in
    Hashtbl.add hst e.pattern.content ck;
    { cpattern = (e.pattern, ck)  ; cexpression = ce } 
  in
  List.map clock_eq el

let clock_io hst ios =
  let aux hst io =
    let ck = (try Hashtbl.find hst io.content with _ -> Base) in
    (io, ck)
  in
  List.map (fun x -> aux hst x) ios  

    

let cl_node node =
  let hst =  Hashtbl.create (List.length node.inputs + List.length node.outputs) in 
  let cname = node.name in 
  let cequations = clock_equations hst node.equations in
  let cinputs = clock_io hst node.inputs in
  let coutputs = clock_io hst node.outputs in
  { cname ; cinputs; coutputs; cequations } 
