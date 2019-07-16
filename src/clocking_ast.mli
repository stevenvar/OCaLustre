open Parsing_ast

type clkvar = { index : int; mutable value : ck }
and clk_scheme = int list * ct

(** AST inspired from "Clock-directed modular code generation" **)

and ident = string

and ct =
  | Ck of ck
  | CkTuple of ct list

and ck =
  | CkBase
  | CkUnknown
  | CkVariable of clkvar
  | Ckon of ck * ident
  | Ckonnot of ck * ident

type cexpression = { ce_desc : cexp_desc ; ce_clk : ct; ce_loc : Location.t }

and cexp_desc =
  | CAlternative of cexpression * cexpression * cexpression
  | CCondact of (bool*ident) list * cexpression
  | CApplication of ident * int * ck * cexpression
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
  | CCall of (ident * cexpression list)
  | CUnit


and cnode = {
    cnode_clock : clk_scheme;
    cinputs_clk : ct;
    coutputs_clk : ct;
    cname : pattern;
    cinputs : pattern;
    coutputs : pattern;
    cequations : cequation list;
}
and cequation = {
  cpattern : pattern;
  cexpression : cexpression;
  cclock : ct;
}
