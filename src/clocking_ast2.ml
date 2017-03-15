open Parsing_ast

type clock =
  | CkUnknown
  | CkVar of { index : int ; mutable value : clock }
  | CkFun of clock * clock
  | CkTuple of clock list
  | CkOn of clock * clock
  | CkOnnot of clock * clock
  | CkCarrier of string * clock

type ck_scheme =
  Forall of int list * clock

type env =
    Env of (string * ck_scheme ) list

type cnode = {
  cname : cpattern;
  cinputs : cpattern;
  coutputs : cpattern;
  cequations : cequation list;
}
and cequation = {
  cpattern : cpattern;
  cexpression : cexpression
}

and cexpression = {
  ce_desc : exp_desc ;
  ce_loc : Location.t;
  ce_clock : clock
}
and cpattern = {
  cp_desc : patt_desc;
  cp_loc : Location.t;
  cp_clock : clock
}

let rec print_list fmt (l,sep) =
  match l with
    [] -> ()
  | [x] -> Format.fprintf fmt "%d" x
  | x::xs -> Format.fprintf fmt "%d%s%a" x sep print_list (xs,sep)

let rec print_clock fmt ck =
  let ck_name ck =
    match ck with
    | CkCarrier (s, _ ) -> s
    | _ ->  failwith "Not a carrier"
  in
  let rec print_tuple fmt cs =
    match cs with
    | [] -> ()
    | [x] -> Format.fprintf fmt "%a" print_clock x
    | x::xs -> Format.fprintf fmt "%a * %a"
                 print_clock x
                 print_tuple xs
  in
  match ck with
  | CkUnknown -> Format.fprintf fmt "?"
  | CkVar { index = i ; value = CkUnknown } ->
    Format.fprintf fmt "%d" i
  | CkVar { index = i ; value = v } ->
    print_clock fmt v
  | CkFun (a,b) -> Format.fprintf fmt "%a -> %a"
                     print_clock a
                     print_clock b
  | CkTuple cs -> print_tuple fmt cs
  | CkOn (a,b) -> Format.fprintf fmt "%a on %s"
                    print_clock a
                    (ck_name b)
  | CkOnnot (a,b) -> Format.fprintf fmt "%a on (not (%s))"
                    print_clock a
                    (ck_name b)
  | CkCarrier (s,c) -> Format.fprintf fmt "(%s : %a)"
                         s
                         print_clock c



let print_clock_scheme fmt (Forall (cs,ck)) =
  Format.fprintf fmt "forall %a . %a"
    print_list (cs,",")
    print_clock ck

let rec cpatt_of_patt { p_desc ; p_loc } cp_clock =
  { cp_desc = p_desc ; cp_loc = p_loc ; cp_clock}
