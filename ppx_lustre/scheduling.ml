open Ast
open Astprinter

type vertice = string * equation 

module S = Set.Make(String)
    
module G = Set.Make(
  struct
    type t = vertice * S.t
    let compare ((s1,_),_) ((s2,_),_) =
      compare s1 s2
  end
  )

let rec get_patt_id p =
  p.content

let rec get_expr_id e s =
  match e with 
  | Variable i -> S.add i.content s
  | Alternative (e1,e2,e3) ->
    let s = get_expr_id e1 s in
    let s = get_expr_id e2 s in 
    get_expr_id e3 s 
  | Application (i,l) ->
    List.fold_left (fun s e -> get_expr_id e s)  s l 
  | InfixOp (op, e1, e2) ->
    let s = get_expr_id e1 s in 
    get_expr_id e2 s
  | PrefixOp (op, e1) ->
    get_expr_id e1 s 
  | Value v -> s


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

let remove_init_dependency g =
  let init =
    S.add ("init") (S.empty)
  in
  G.fold
    (fun ((y,e),s) g -> G.add ((y,e),S.diff s init) g)
    g G.empty

let remove_inputs_dependency g inputs =
  let inputs' =
    List.fold_left (fun l x -> S.add x.content l) (S.empty) inputs
  in
  G.fold (fun ((y,e),s) g -> G.add ((y,e),S.diff s inputs') g)
    g G.empty


let rec toposort topo g name =
  if G.is_empty g then List.rev topo
  else
    let g1 , g2 = G.partition (fun ((_,_),s) -> S.is_empty s) g in
    if G.is_empty g1 then
      Error.print_error name.loc ("Causality loop in node "^name.content) ;
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
    toposort topo g name

let schedule node =
  let inputs = node.inputs in
  let eqs = node.equations in
  let g =
    List.fold_left
      (fun g eq ->
         let pv = get_patt_id eq.pattern in
         let ev = get_expr_id eq.expression (S.empty) in
         G.add ((pv,eq),ev) g)
      (G.empty) eqs
  in
  (* let g = remove_init_dependency g  in *) 
  let g = remove_inputs_dependency g inputs
  in 
  let eqs = toposort [] g node.name in 
  {
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs
  }
