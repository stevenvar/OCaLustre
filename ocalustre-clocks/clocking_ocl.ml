open Clocked_ast
open Parsing_ast
open Parsing_ocl

let get_constant e =
match e.e_desc with
| Value v -> v
| _ -> Error.print_error e.e_loc "This must be a constant"

let get_ident e =
  match e.e_desc with
  | Variable i -> i
  | _ -> Error.print_error e.e_loc "This must be a constant"


let rec get_clock hst e =
  match e.e_desc with
  | Variable i ->
    (try
       Hashtbl.find hst i
     with _ -> Base)
  | Alternative (e1, e2, e3) -> get_clock hst e2
  | Application (i,el) -> get_clock hst (List.hd el)
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

let rec clock_err cl loc =
  match cl with
  | [Base]::t -> clock_err t loc
  | [On (c,i)]::_ -> Error.print_error loc "Incoherent clock"
  | _ -> failwith "no clocks"

let rec clock_eq l1 l2 =
  match l1 , l2 with
  | [] , [] -> assert false
  | [c1] , [c2] -> c1 = c2
  | h1::t1 , h2::t2 -> h1 = h2 && clock_eq t1 t2
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
  | Application (i,el) ->
    let cel = List.map (fun e -> clock_exp hst e) el in
    { ce_desc = CApplication (i, cel) ; ce_loc = e.e_loc ; ce_clock = [Base] } (* TODO *)
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
    let i = get_ident e2 in
    let ck = On (List.hd ce1.ce_clock,i) in
    let e' = CWhen (ce1,i) in
    { ce_desc = e' ; ce_loc = e.e_loc ; ce_clock = [ck]}
  | Unit ->
    { ce_desc = CUnit ; ce_loc = e.e_loc ; ce_clock = [Base]}
  | Pre e ->
    let ce = clock_exp hst e in
    { ce_desc = CPre ce ; ce_loc = e.e_loc ; ce_clock = ce.ce_clock }


let clock_equations hst el =
  let clock_eq e =
    let ce = clock_exp hst e.expression in
    begin match e.pattern.p_desc with
      | Ident i -> Hashtbl.add hst i (List.hd ce.ce_clock)
      | Tuple t -> failwith "todo"
    end;
    let cpat = { cp_desc = e.pattern.p_desc;
                 cp_loc = e.pattern.p_loc;
                 cp_clock = ce.ce_clock}
    in
    { cpattern = cpat ; cexpression = ce }
  in
  List.map clock_eq el

let clock_io hst ios =
  let aux hst io =
    match io.p_desc with
    | Ident i ->  let ck = (try Hashtbl.find hst i with _ -> Base) in
      { cp_desc = io.p_desc ; cp_loc = io.p_loc ; cp_clock = [ck]}
    | Tuple t -> failwith "todo"
  in
  List.map (fun x -> aux hst x) ios

let cl_node node =
  let hst =  Hashtbl.create (List.length node.inputs + List.length node.outputs) in
  let cequations = clock_equations hst node.equations in
  let cinputs = clock_io hst node.inputs in
  let coutputs = clock_io hst node.outputs in
  let clocks_in = List.map (fun x -> x.cp_clock) cinputs in
  let clocks_out = List.map (fun x -> x.cp_clock) coutputs in
  let ll = List.flatten (clocks_in@clocks_out) in
  let cname = { cp_desc = node.name.p_desc ; cp_loc = node.name.p_loc ; cp_clock = ll} in
  { cname ; cinputs; coutputs; cequations }
