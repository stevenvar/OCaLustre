open Parsing_ast


type clock =
  | Clock_exp of ct
  | Clock_scheme of scheme
and
  ct =
  | CtUnknown
  | CVar of varclock
  | CTuple of ct list
  | Arrow of ct * ct
  | On of ct * carrier
  | Onnot of ct * carrier
  | Carrier of carrier
and varclock = { c_index : int ; mutable c_value : ct }
and scheme = Forall of int list * int list *  ct (* arrow ? *)
and carrier = { carr_index : int ; mutable carr_value : ct }

and cnode = {
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
  cp_desc : cpatt_desc;
  cp_loc : Location.t;
  cp_clock : clock
}
and cpatt_desc =
  | CkIdent of ident
  | CkTuple of cpattern list
  | CkPUnit

(*)
and cexp_desc =
  | CAlternative of cexpression * cexpression * cexpression
  | CApplication of ident * cexpression
  | CInfixOp of inf_operator * cexpression * cexpression
  | CPrefixOp of pre_operator * cexpression
  | CValue of constant
  | CVariable of string
  | CFby of constant * cexpression
  | CArrow of cexpression * cexpression
  | CWhen of cexpression * ident
  | CWhennot of cexpression * ident
  | CPre of cexpression
  | CETuple of cexpression list
  | CMerge of cexpression * cexpression * cexpression
  | CUnit
*)
