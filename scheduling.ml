open Parsing_ast
open Parsing_ast_printer
open Parsing_ocl


type vertice = string * equation

module S = Set.Make(String)

(* We use a graph to represent dependences *)
module G = Set.Make(
  struct
    type t = vertice * S.t
    let compare ((s1,_),_) ((s2,_),_) =
      compare s1 s2
  end)

(* get the ids of each construct in order to
 * determine what are the dependences of the
 * caller *)
let rec get_patt_id { p_desc ; p_loc } =
  match p_desc with
  | Ident i -> [i]
  | Tuple t -> List.flatten (List.map get_patt_id t)

let rec get_expr_id e s =
  match e.e_desc with
  | Variable i -> S.add i s
  | Alternative (e1,e2,e3) ->
    let s = get_expr_id e1 s in
    let s = get_expr_id e2 s in
    get_expr_id e3 s
  | Application (i,el) ->
     List.fold_left (fun accu e -> get_expr_id e accu) s el
  | InfixOp (op, e1, e2) ->
    let s = get_expr_id e1 s in
    get_expr_id e2 s
  | PrefixOp (op, e) ->
    get_expr_id e s
  | Value v -> s
  | Fby (v,e) -> s (* not dependent on e since it appears at the next instant *)
  | Arrow (v,e) -> s
  | When (e,i) -> get_expr_id e s
  | Unit -> s
  | Pre e -> get_expr_id e s
  | ETuple el -> List.fold_left (fun accu e -> get_expr_id e accu) s el

(* make the graph *)
let mk_dep_graph eqs =
  let eq_dep eq =
    let dep = get_expr_id eq.expression (S.empty) in
    ((eq.pattern,eq),dep)
  in
  List.map (fun x -> (eq_dep x)) eqs


let print_set fmt s =
  S.iter (fun x -> Format.fprintf fmt " %s" x ) s


let print_graph fmt g =
  G.iter (fun ((x,_),s) -> Format.fprintf fmt " %s -> %a " x print_set s ) g

(* useless now *)
let remove_init_dependency g =
  let init =
    S.add ("init") (S.empty)
  in
  G.fold
    (fun ((y,e),s) g -> G.add ((y,e),S.diff s init) g)
    g G.empty

(* equations do not depend (for scheduling) on the
 * inputs *)

let remove_inputs_dependency g inputs =
  let inputs' =
    List.fold_left (fun l x -> S.add (List.hd (get_patt_id x)) l) (S.empty) inputs
  in
  G.fold (fun ((y,e),s) g -> G.add ((y,e),S.diff s inputs') g)
    g G.empty

(* reverse topological sort of the graph = order of the dependencies *)
let rec toposort topo g name loc =
  if G.is_empty g then List.rev topo
  else
    let g1 , g2 = G.partition (fun ((_,_),s) -> S.is_empty s) g in
    if G.is_empty g1 then
      let vars = G.fold (fun ((s,e),_) l -> s^" "^l) g "" in
      Error.print_error loc
        ("Causality loop in node "^name^" including these variables : "^vars )
    else
    let sv =
      G.fold (fun ((x,_),_) s -> S.add x s) g1 S.empty
    in
    let g =
      G.fold
        (fun ((y,e),s) g -> G.add ((y,e),S.diff s sv) g)
        g2 G.empty
    in
    let topo =
      G.fold
        (fun ((_,e),s) l -> if List.mem e l then l else e::l)
        g1 topo
    in
    toposort topo g name loc

let schedule node =
  let inputs = node.inputs in
  let eqs = node.equations in
  let g =
    List.fold_left
      (fun g eq ->
         let pv = get_patt_id eq.pattern in
         let ev = get_expr_id eq.expression (S.empty) in
         List.fold_left (fun g x -> G.add ((x,eq),ev) g) g pv )
      (G.empty) eqs
  in
  let g = remove_inputs_dependency g inputs
  in
  let name = List.hd (get_patt_id node.name) in
  let eqs = toposort [] g name node.name.p_loc in
  {
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs
  }
