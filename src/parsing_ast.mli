

type ident = string
type  condition = expression option
and  node = {
  pre : condition;
  post : condition;
  inv : condition;
  name : pattern;
  inputs : pattern;
  outputs : pattern;
  equations :  equation list;
}
and  equation = {
  pattern : pattern ;
  expression : expression;
}
and value = Integer of int
          | Float of float
          | Bool of bool
          | String of string
          | Enum of string
          | Nil

and expression = {
  e_desc : exp_desc;
  e_loc : Location.t
}
and pattern = {
  p_desc : patt_desc;
  p_loc : Location.t
}
and patt_desc =
  | Ident of ident
  | Typed of pattern * string
  | Tuple of pattern list
  | PUnit

and exp_desc =
  | Alternative of expression * expression * expression
  | Application of ident * expression
  | InfixOp of inf_operator * expression * expression
  | PrefixOp of pre_operator * expression
  | Value of value
  | Variable of ident
  | Array of expression list
  | Imperative_update of expression * ((expression * expression) list)
  | Fby of expression * expression
  | When of expression * expression
  | Whennot of expression * expression
  | ETuple of expression list
  | Pre of expression
  (* | Current of expression *)
  | Arrow of expression * expression
  | Merge of expression * expression * expression
  | Call of Parsetree.expression
  | Unit
and inf_operator =
  | Diff
  | Equals
  | Plus
  | Minus
  | Times
  | Div
  | Plusf
  | Minusf
  | Timesf
  | Divf
  | Inf
  | Sup
  | Infe
  | Supe
  | Bor
  | Band
  | Mod

and pre_operator =
  | Not
  | Neg
  | Negf
