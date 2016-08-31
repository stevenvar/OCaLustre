open Clocking_ast
open Parsing_ast
open Parsing_ocl

let get_constant e =
match e.e_desc with
| Value v -> v
| _ -> Error.print_error e.e_loc "This must be a constant"

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be an ident"


let rec get_clock hst e =
  match e.e_desc with
  | Variable i ->
    (try
       Hashtbl.find hst i
     with _ -> Base)
  | Alternative (e1, e2, e3) -> get_clock hst e2
  | Application (i,e) -> get_clock hst e
  | InfixOp (op, e1, e2) -> get_clock hst e1
  | PrefixOp (op, e) -> get_clock hst e
  | Value v -> Base
  | Fby (v,e) -> get_clock hst e
  | Arrow (v,e) -> get_clock hst e
  | When (e1, e2) ->
    failwith "todo"
  (*)  On ((get_clock hst e), e2) (* the clock is in the expression *) *)
  | Unit -> Base
  | Pre e -> get_clock hst e
  | ETuple el -> get_clock hst (List.hd el) (* TODO *)

let rec clock_err cl loc =
  match cl with
  | [Base]::t -> clock_err t loc
  | [On (c,i)]::_ -> Error.print_error loc "Incoherent clock"
  | _ -> failwith "no clocks"

let rec clock_eq l1 l2 =
  match l1 , l2 with
  | [] , [] -> assert false
  | [c1] , [c2] -> c1 = c2
  | h1::t1 , h2::t2 -> h1 = Base || h2 = Base || h1 = h2 && clock_eq t1 t2
  | _ -> false


let const_of_exp e =
  match e with
  | Value v -> v
  | _ -> failwith "not a constant"

let rec clock_exp hst e =
  match e.e_desc with
  | Variable i ->
    let ck = try Hashtbl.find hst i with
        _ -> Base in
    { ce_desc = CVariable i ; ce_loc = e.e_loc ; ce_clock = [ck] }
  | Application (i,e) ->
    let e' = clock_exp hst e in
    { ce_desc = CApplication (i, e') ; ce_loc = e.e_loc ; ce_clock = [Base] } (* TODO *)
  | Alternative (e1,e2,e3) ->
    let ce1 = clock_exp hst e1 in
    let ce2 = clock_exp hst e2 in
    let ce3 = clock_exp hst e3 in
    let e' = CAlternative (ce1, ce2, ce3) in
    if (clock_eq ce1.ce_clock ce2.ce_clock &&
        clock_eq ce2.ce_clock ce3.ce_clock)
    then { ce_desc = e' ; ce_loc = e.e_loc ; ce_clock = ce1.ce_clock }
    else
      clock_err [ce1.ce_clock;ce2.ce_clock;ce3.ce_clock] e.e_loc
  | InfixOp (op, e1, e2) ->
    let ce1 = clock_exp hst e1 in
    let ce2 = clock_exp hst e2 in
    let e' = CInfixOp (op, ce1, ce2) in
    if clock_eq ce1.ce_clock ce2.ce_clock  then
      { ce_desc = e' ; ce_loc = e.e_loc ; ce_clock = ce1.ce_clock }
    else
      clock_err [ce1.ce_clock;ce2.ce_clock] e.e_loc
  | PrefixOp (op, e') ->
    let ce = clock_exp hst e' in
    let e' = CPrefixOp (op, ce) in
    { ce_desc = e' ; ce_loc = e.e_loc ; ce_clock = ce.ce_clock }
  | Value v ->
    { ce_desc = CValue v ; ce_loc = e.e_loc ; ce_clock = [Base] }
  | Fby (e1,e2) ->
    let ce = clock_exp hst e2 in
    let v = get_constant e1 in
    let e' = CFby (v, ce) in
    { ce_desc = e'; ce_loc = e.e_loc ; ce_clock = ce.ce_clock}
  | Arrow (e1,e2) ->
    let ce1 = clock_exp hst e1 in
    let ce2 = clock_exp hst e2 in
    let e' = CArrow (ce1, ce2)  in
    { ce_desc = e'; ce_loc = e.e_loc ; ce_clock = ce1.ce_clock}
  | When (e1,e2) ->
    let ce1 = clock_exp hst e1 in
    let ce2 = clock_exp hst e2 in
    let i = get_ident e2 in
    let ck = On (List.hd ce1.ce_clock,i) in
    let e' = CWhen (ce1,i) in
    if (clock_eq ce1.ce_clock ce2.ce_clock) then
      { ce_desc = e' ; ce_loc = e.e_loc ; ce_clock = [ck]}
    else
      clock_err [ce1.ce_clock;ce2.ce_clock] e.e_loc
  | Unit ->
    { ce_desc = CUnit ; ce_loc = e.e_loc ; ce_clock = [Base]}
  | Pre e ->
    let ce = clock_exp hst e in
    { ce_desc = CPre ce ; ce_loc = e.e_loc ; ce_clock = ce.ce_clock }
  | ETuple el ->
    let cel = List.map (fun e -> clock_exp hst e) el in
    { ce_desc = CETuple(cel) ; ce_loc = e.e_loc ; ce_clock = [Base] } (* TODO *)

let rec cpat_of_pat ck p =
  match p.p_desc with
  | Ident i -> { cp_desc = CIdent i ; cp_loc = p.p_loc ; cp_clock = ck }
  | Tuple t ->
    let pl = List.map (cpat_of_pat ck) t in
    { cp_desc = CTuple pl ; cp_loc = p.p_loc ; cp_clock = ck }
  | PUnit -> { cp_desc = CPUnit ; cp_loc = p.p_loc ; cp_clock = ck }


let clock_equations hst el =
  let rec aux hst ck p=
  begin match p.p_desc with
    | Ident i -> Hashtbl.add hst i ck
    | Tuple t -> List.iter (aux hst ck) t
    | PUnit -> ()
  end;
  in
  let rec clock_eq e =
    let ce = clock_exp hst e.expression in
    aux hst (List.hd ce.ce_clock) e.pattern;

    let cpat = cpat_of_pat ce.ce_clock e.pattern
    in
    { cpattern = cpat ; cexpression = ce }
  in
  List.map clock_eq el

let clock_io hst ios =
  let aux hst io =
    match io.p_desc with
    | Ident i ->  let ck = (try Hashtbl.find hst i with _ -> Base) in
      cpat_of_pat [ck] io
    | Tuple t -> cpat_of_pat [Base] io (*todo*)
    | PUnit -> cpat_of_pat [Base] io
  in
  List.map (fun x -> aux hst x) ios

let cl_node node =
  let hst =  Hashtbl.create (2) in
  let cequations = clock_equations hst node.equations in
  let cinputs = clock_io hst [node.inputs] in
  let coutputs = clock_io hst [node.outputs] in
  let clocks_in = List.map (fun x -> x.cp_clock) cinputs in
  let clocks_out = List.map (fun x -> x.cp_clock) coutputs in
  let ll = List.flatten (clocks_in@clocks_out) in
  let cname = cpat_of_pat ll node.name in
  { cname ; cinputs; coutputs; cequations }
