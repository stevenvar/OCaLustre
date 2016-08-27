open Parsing_ast
open Parsing_ocl
open Parsing_ast_printer
exception CycleFound of ident list

let rec get_dep_id e l  =
  match e.e_desc with
  | Variable i -> i::l
  | Alternative (e1,e2,e3) ->
    let l = get_dep_id e1 l in
    let l = get_dep_id e2 l in
    get_dep_id e3 l
  | Application (i,el) ->
    List.fold_left (fun accu e -> get_dep_id e accu) l el
  | InfixOp (op, e1, e2) ->
    let l = get_dep_id e1 l in
    get_dep_id e2 l
  | PrefixOp (op, e) ->
    get_dep_id e l
  | Value v -> l
  | Fby (v,e) -> l (* not dependent on e since it appears at the next instant *)
  | Arrow (v,e) -> l
  | When (e,i) -> get_dep_id e l
  | Unit -> l
  | Pre e -> get_dep_id e l
  | ETuple el -> List.fold_left (fun accu e -> get_dep_id e accu) l el


let rec get_id p =
  match p.p_desc with
  | Ident i -> i
  | Tuple t -> failwith "tuple"

let rec contains e i =
  match e.pattern.p_desc with
  | Ident j -> i = j
  | Tuple t -> List.fold_left (fun acc p' ->  contains { pattern = p' ; expression = { e_desc = Unit ; e_loc = Location.none }} i || acc) false t

let rec find_eq_from_id i eqs =
  match eqs with
  | [] -> failwith "no equation"
  | h::t -> if contains h i then h else find_eq_from_id i t

let rec get_ids p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple t ->
    let ids = List.map get_ids t in
    List.flatten ids

let mk_dep_graph (eqs : equation list) =
  let eq_dep eq =
    let dep = get_dep_id eq.expression [] in
    let ids = get_ids eq.pattern in
    List.map (fun i -> (i,dep) ) ids
  in
  List.map (fun x -> (eq_dep x)) eqs |> List.flatten

let rec print_dep_graph g =
  let print_one_dep fmt (e,dep) =
    print_string "(";
    print_ident fmt e;
    print_string "-->[";
    List.iter (fun s -> print_ident fmt s) dep;
    print_endline "])"
  in
  match g with
  | [] -> ()
  | h::t -> print_one_dep Format.std_formatter h; print_dep_graph t

let dfs (graph : (ident * ident list) list) visited start_node =
  let rec explore path visited node =
    if List.mem node path    then raise (CycleFound path) else
    if List.mem node visited then visited else
      let new_path = node :: path in
      let edges    = List.assoc node graph in
      let visited  = List.fold_left (explore new_path) visited edges in
      node :: visited
  in explore [] visited start_node

let toposort graph =
  List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph

let diff l1 l2 = List.filter (fun x -> not (List.mem x l2)) l1

let rec remove_inputs_dep inputs g =
  let aux (i,il) =
    (i, diff il inputs)
  in
  match g with
  | [] -> []
  | h::t -> aux h :: remove_inputs_dep inputs t

let rec remove_dups lst= match lst with
  | [] -> []
  | h::t -> h::(remove_dups (List.filter (fun x -> x<>h) t))

let schedule node =
  let inputs = List.map get_ids node.inputs |> List.flatten in
  let eqs = node.equations in
  try
    let g = mk_dep_graph eqs in
    let g = remove_inputs_dep inputs g in
  let ids_sorted = List.rev (toposort g) in
  let eqs_sorted= List.map (fun i -> find_eq_from_id i eqs) ids_sorted in
  let eqs_sorted = remove_dups eqs_sorted in
  {
    name = node.name;
    inputs = node.inputs;
    outputs = node.outputs;
    equations = eqs_sorted
  }
  with CycleFound p ->
    let vars = List.fold_left (fun acc i -> i^" "^acc) "" p in
    let name = List.hd (get_ids node.name) in
        Error.print_error node.name.p_loc
          ("Causality loop in node "^name^" with these variables : "^vars )
