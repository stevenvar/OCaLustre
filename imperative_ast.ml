
open Parsetree
open Parsing_ast
open Clocking_ast

type app_inits = (pattern * imp_expr) list
and init = pattern * imp_expr
and imp_inits = init list
and imp_expr =
  | IValue of constant
  | IConstr of string
  | IVariable of ident
  | IApplication of ident * int * imp_expr
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IETuple of imp_expr list
  | IUnit
and
  imp_infop =
  | IEquals
  | IDiff
  | IPlus
  | IMinus
  | ITimes
  | IDiv
  | IPlusf
  | IMinusf
  | IDivf
  | ITimesf
and
  imp_preop =
  | INot
  | INeg
  | INegf

type imp_equation =  {
  i_pattern : pattern;
  i_expression : imp_expr;
}

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (pattern * imp_expr) list;
}

type imp_node = {
  i_name : ident;
  i_inputs : pattern;
  i_outputs : pattern;
  i_inits : imp_inits;
  i_step_fun : imp_step;
}
