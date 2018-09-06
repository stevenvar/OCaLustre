

type ident = string

and node = {
  name : pattern;
  inputs : pattern list;
  outputs : pattern list;
  equations :  equation list;
}

and exp_desc =
  | IntOp of int_operator * expression * expression
  | BoolOp of bool_operator * expression * expression
  | FloatOp of float_operator * expression * expression
  | CompOp of comp_operator * expression * expression
  | Not of expression
  | Unit
  | Constant of value
  | Constructor of ident
  | Opposite of expression
  | Oppositef of expression
  | When of expression * expression
  | Whennot of expression * expression
  | Eval of Parsetree.expression


and cexp_desc =
  | Merge of ident * cexpression * cexpression
  | If of expression * cexpression * cexpression
  | Exp of expression

and equation =
  | Cexp of pattern * cexpression
  | Fby of pattern * value * cexpression
  | App of pattern list * ident * cexpression list

and value = Integer of int
          | Float of float
          | Bool of bool

and cexpression = {
  ce_desc : cexp_desc;
  ce_loc : Location.t
}

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
  | PUnit

and int_operator =
  | Plus
  | Minus
  | Times
  | Div
  | Mod

and float_operator =
  | Plusf
  | Minusf
  | Timesf
  | Divf

and comp_operator =
  | Equals
  | Nequals
  | Inf
  | Sup
  | Infe
  | Supe

and bool_operator =
  | Or
  | And
