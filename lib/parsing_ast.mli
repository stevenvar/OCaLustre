

type ident = string
type condition = expression option 
and node = {
  pre : condition;
  post : condition;
  inv : condition; 
  name : pattern;
  inputs : pattern;
  outputs : pattern;
  equations : equation list;
}
and equation = {
  pattern : pattern ;
  expression : expression;
}
and constant = Integer of int | Float of float | Bool of bool | Enum of string | Nil
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
  | Tuple of pattern list
  | PUnit

and exp_desc =
  | Alternative of expression * expression * expression
  | Application of ident * expression
  | InfixOp of inf_operator * expression * expression
  | PrefixOp of pre_operator * expression
  | Value of constant
  | Variable of ident
  | Fby of expression * expression
  | When of expression * expression
  | Whennot of expression * expression
  | ETuple of expression list
  (* | Current of expression *)
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

and pre_operator =
  | Not
  | Neg
  | Negf
