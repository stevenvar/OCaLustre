open Parsing_ast
open Parsing_ast_printer

exception CycleFound of ident list

(** Get all ids that an expression is dependent on **)
let rec get_dep_id e l  =
  match e.e_desc with
  | Variable i -> i::l
  | Alternative (e1,e2,e3) ->
    let l' = get_dep_id e1 l in
    let l'' = get_dep_id e2 l' in
    get_dep_id e3 l''
  | Application (_i,_num,e) ->
    get_dep_id e l
  | Call (_f,e) ->
    get_dep_id e l
  | InfixOp (_op, e1, e2) ->
    let l = get_dep_id e1 l in
    get_dep_id e2 l
  | PrefixOp (_op, e) ->
    get_dep_id e l
  | Value _v -> l
  | Array _el -> l
  | Array_fold (e,_f,e') ->
    let l = get_dep_id e l in
    get_dep_id e' l
  | Array_map (e,_f) ->
    get_dep_id e l
  | Array_get (e,e') ->
    let l = get_dep_id e l in
    get_dep_id e' l
  | Imperative_update (e,el) ->
    let l = get_dep_id e l in
    let flat = List.fold_left (fun acc (e1,e2) -> e1::e2::acc) [] el in
    List.fold_left (fun accu e -> get_dep_id e accu) l flat
  | Arrow (e1,e2) ->
    let l = get_dep_id e1 l in
    get_dep_id e2 l
  | Pre _e ->  l
  | Fby (v,_e) ->
    (* not dependent on e since it appears at the next instant *)
    get_dep_id v l
  | Clock e ->
    get_dep_id e l
  | When (e,i) ->
    let l = get_dep_id i l in
    get_dep_id e l
  | Whennot (e,i) -> get_dep_id e (get_dep_id i l)
  | Unit -> l
  | ETuple el -> List.fold_left (fun accu e -> get_dep_id e accu) l el
  | Merge (e1,e2,e3) ->
    let l = get_dep_id e1 l in
    let l = get_dep_id e2 l in
    get_dep_id e3 l

let rec get_id p =
  match p.p_desc with
  | Ident i -> i
  | Tuple _t -> Error.print_error p.p_loc "Not an ident"
  | PUnit -> Error.print_error p.p_loc "Not an ident"
  | Typed (p,_t) -> get_id p

let rec contains e i =
  match e.pattern.p_desc with
  | Ident j -> i = j
  | Tuple t ->
    List.fold_left (fun acc p' ->
        contains { pattern = p' ;
                   expression = { e_desc = Unit ;
                                  e_loc = Location.none }}
          i || acc) false t
  | PUnit -> false
  | Typed (p,_t) ->
    contains { pattern = p ;
               expression = { e_desc = Unit ;
                              e_loc = Location.none }} i

let rec find_eq_from_id i eqs =
  match eqs with
  | [] -> failwith ("no equation : "^i)
  | h::t -> if contains h i then h else find_eq_from_id i t

let rec get_ids p =
  match p.p_desc with
  | Ident i -> [i]
  | Tuple t ->
    let ids = List.map get_ids t in
    List.flatten ids
  | PUnit -> []
  | Typed (p,_s) -> get_ids p

let mk_dep_graph (eqs : equation list) =
  let eq_dep eq =
    let dep = get_dep_id eq.expression [] in
    let ids = get_ids eq.pattern in
    List.map (fun i -> (i,dep) ) ids
  in
  List.map (fun x -> (eq_dep x)) eqs |> List.flatten

let rec print_dep_graph g =
  let rec print_idents fmt l =
    match l with
      [] -> ()
    | [x] -> Format.fprintf fmt "%a" print_ident x
    | x::xs -> Format.fprintf fmt "%a, %a" print_ident x print_idents xs in
  let print_one_dep fmt (e,dep) =
    Format.fprintf fmt
      "%a depends on (%a)\n"
      print_ident e
      print_idents dep
  in
  match g with
  | [] -> ()
  | h::t -> print_one_dep Format.std_formatter h; print_dep_graph t

let dfs (graph : (ident * ident list) list) visited start_node =
  let rec explore path visited node =
    if List.mem node path    then raise (CycleFound path) else
    if List.mem node visited then visited else
      let new_path = node :: path in
      let edges = try List.assoc node graph
        with Not_found -> failwith ("Variable not found : "^node);  in
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

let schedule_eqs eqs inputs =
  let g = mk_dep_graph eqs in
  let g = remove_inputs_dep inputs g in
  let ids_sorted = List.rev (toposort g) in
  let eqs_sorted= List.map (fun i -> find_eq_from_id i eqs) ids_sorted in
  remove_dups eqs_sorted

let schedule node =
  let inputs = get_ids node.inputs in
  try
    let eqs_sorted = schedule_eqs node.equations inputs in
    {
      pre = node.pre;
      post = node.post;
      inv = node.inv;
      name = node.name;
      inputs = node.inputs;
      outputs = node.outputs;
      equations = eqs_sorted
    }
  with CycleFound p ->
    let print_vars fmt vs = Tools.print_list fmt (fun _ ->
        (Format.fprintf fmt "%s")) vs in
    let s = Format.asprintf "Causality loop in node \"%a\" with variables %a"
        print_pattern node.name
        print_vars p
    in
    Error.print_error node.name.p_loc s
