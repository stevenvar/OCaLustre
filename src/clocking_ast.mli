open Parsing_ast

type carrier = UnknownCar | VarCar of varcar

and varcar = { cindex : int; mutable cvalue : carrier }

type clock = Unknown
           | Var of varclock
           | CTuple of clock list
           | Arrow of clock * clock
           | On of clock * carrier
           | Onnot of clock * carrier
           | Carrier of carrier * clock (* clocks with a name like c :: 'a *)

and cexp_desc =
  | CAlternative of cexpression * cexpression * cexpression
  | CCondact of bool * ident * cexpression
  | CApplication of ident * int * cexpression
  | CInfixOp of inf_operator * cexpression * cexpression
  | CPrefixOp of pre_operator * cexpression
  | CValue of value
  | CVariable of ident
  | CArray of cexpression list
  | CArray_get of cexpression * cexpression
  | CArray_fold of cexpression * Parsetree.expression * cexpression
  | CArray_map of cexpression * Parsetree.expression
  | CImperative_update of cexpression * ((cexpression * cexpression) list)
  | CFby of cexpression * cexpression
  | CWhen of cexpression * cexpression
  | CWhennot of cexpression * cexpression
  | CETuple of cexpression list
  | CPre of cexpression
  | CArrow of cexpression * cexpression
  | CMerge of cexpression * cexpression * cexpression
  | CCall of Parsetree.expression
  | CUnit


and varclock = { index : int ; mutable value : clock }
(* clocks vars * carriers vars * clock *)
and clock_scheme = Forall of int list * int list * clock

and cnode = {
    cnode_clock : clock_scheme;
    cname : pattern;
    cinputs : pattern;
    coutputs : pattern;
    cequations : cequation list;
}
and cequation = {
  cpattern : pattern;
  cexpression : cexpression
}

and cexpression = {
  ce_desc : cexp_desc ;
  ce_loc : Location.t;
  ce_clock : clock;
  ce_carrier : carrier
}
and cpattern = {
  cp_desc : cpatt_desc;
  cp_loc : Location.t;
  cp_clock : clock_scheme
}
and cpatt_desc =
  | CkIdent of ident
  | CkTuple of cpattern list
  | CkPUnit
