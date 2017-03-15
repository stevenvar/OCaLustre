open Parsing_ast


type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | CtUnknown
  | CVar of varclock
  | CTuple of ct list
  | CTyped of ct * string
  | Arrow of ct * ct
  | On of ct * carrier
  | Onnot of ct * carrier
  | Carrier of carrier * ct
and varclock = { c_index : int ; mutable c_value : ct }
and scheme = Forall of int list * string list *  ct (* arrow ? *)
(* and carrier = { carr_index : int ; mutable carr_value : ct } *)
and carrier = string

and cnode = {
  cname : string;
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
  cp_desc : cpatt_desc;
  cp_loc : Location.t;
  cp_clock : clock
}
and cpatt_desc =
  | CkIdent of ident
  | CkTuple of cpattern list
  | CkPUnit
