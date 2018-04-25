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
  | Tuple t -> "tuple"
  | _ ->
    let s = Format.asprintf "get_ident :: %a : not an ident" Parsing_ast_printer.print_pattern p in
    Error.print_error p.p_loc s

let rec ident_of_cexpr ce =
  let open Clocking_ast in
  match ce.ce_desc with
  | CVariable s -> s
  | _ -> failwith "ident_of_cexpr"


open Parsetree
open Longident
open Asttypes
let stringloc_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = prefix^i^suffix;
    loc = Location.none;
  }


let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i^suffix);
    loc = Location.none
  }


let rec string_of_pattern ?(prefix="") ?(suffix="") p =
  let open Parsing_ast in
  match p.p_desc with
  | Ident i -> prefix^i^suffix;
  | Typed (p,t) -> string_of_pattern p
  | PUnit -> "()"
  | _ -> failwith "no tuple !"


let stringloc_of_pattern ?(prefix="") ?(suffix="") p =
  let open Parsing_ast in
  match p.p_desc with
  | Ident i ->
    {
      txt = prefix^i^suffix;
      loc = p.p_loc
    }
  | _ -> failwith "no tuple !"

let rec expr_of_pattern p =
  let open Parsing_ast in
  let open Ast_helper in
  match p.p_desc with
  | PUnit -> [%expr () ]
  | Ident x -> Ast_convenience.evar x
  | Tuple t -> Ast_convenience.tuple (List.map expr_of_pattern t)
  | Typed (p,s) -> expr_of_pattern p

let rec pat_of_pattern p =
  let open Parsing_ast in
  match p.p_desc with
  | Ident i -> { ppat_desc = Ppat_var (stringloc_of_pattern p) ;
                 ppat_loc = p.p_loc ;
                 ppat_attributes = [] }
  | Tuple t ->
    let tl = List.map (fun p -> pat_of_pattern p) t in
    { ppat_desc = Ppat_tuple tl ;
      ppat_loc = p.p_loc ;
      ppat_attributes = [] }
  | PUnit -> { ppat_desc = Ppat_construct (lid_of_ident "()" ,None);
               ppat_loc = p.p_loc ;
               ppat_attributes = [] }
  | Typed (p,s) ->
    let core_type = {
       ptyp_desc = Ptyp_constr(lid_of_ident s,[]);
     ptyp_loc =  p.p_loc ;
     ptyp_attributes = [];
    }
    in
    {
      ppat_desc = Ppat_constraint (pat_of_pattern p, core_type) ;
      ppat_loc = p.p_loc;
      ppat_attributes = []}
