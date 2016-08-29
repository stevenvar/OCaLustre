open Parsing_ast

type clock = Base | On of clock * ident
and clk = clock list
and cnode = {
  cname : cpattern;
  cinputs : cpattern list;
  coutputs : cpattern list;
  cequations : cequation list;
}
and cequation = {
  cpattern : cpattern;
  cexpression : cexpression}
and cexpression = {
  ce_desc : cexp_desc ;
  ce_loc : Location.t;
  ce_clock : clk
}
and cpattern = {
  cp_desc : cpatt_desc;
  cp_loc : Location.t;
  cp_clock : clk
}

and cpatt_desc =
  | CIdent of ident
  | CTuple of cpattern list

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
  | CPre of cexpression
  | CETuple of cexpression list
  | CUnit
