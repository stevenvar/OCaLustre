let print_list fmt ?sep:(sep=", ") f l =
  let rec aux fmt l =
  match l with
  | [] -> ()
  | [h] -> Format.fprintf fmt "%a" f h
  | h::t -> Format.fprintf fmt "%a%s%a" f h sep aux t
  in
  Format.fprintf fmt  "(%a)" aux l

let rec prefix_pattern ?pre:(pre="st_") p =
  let open Parsing_ast in
  let rec new_desc = match p.p_desc with
    | Ident i -> Ident (pre^i)
    | Tuple t -> Tuple (List.map (fun x -> prefix_pattern ~pre:pre x) t)
    | PUnit -> PUnit
    | Typed (p',s) -> (prefix_pattern ~pre:pre p').p_desc
  in
  { p with p_desc = new_desc }

let rec suffix_pattern ?suf:(suf="st_") p =
  let open Parsing_ast in
  let rec new_desc = match p.p_desc with
    | Ident i -> Ident (i^suf)
    | Tuple t -> Tuple (List.map (fun x -> suffix_pattern ~suf:suf x) t)
    | PUnit -> PUnit
    | Typed (p',s) -> (suffix_pattern ~suf:suf p').p_desc
  in
  { p with p_desc = new_desc }

let rec get_ident p =
  let open Parsing_ast in
  match p.p_desc with
  | Ident i -> i
  | Typed (p,s) -> get_ident p
  | _ ->
    let s = Format.asprintf "%a : not an ident" Parsing_ast_printer.print_pattern p in
    Error.print_error p.p_loc s

let rec ident_of_cexpr ce =
  let open Clocking_ast in
  match ce.ce_desc with
  | CVariable s -> s
  | _ -> failwith "ident_of_cexpr"
