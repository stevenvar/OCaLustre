
open Parsetree
open Parsing_ast
open Clocking_ast

type app_inits = (cpattern * imp_expr) list
and init = cpattern * imp_expr
and imp_inits = init list
and imp_expr =
  | IValue of constant
  | IConstr of string
  | IVariable of ident
  | IApplication of ident * imp_expr list
  | ITuple of imp_expr list
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
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

type imp_equation =  {
  i_pattern : cpattern;
  i_expression : imp_expr;
}

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (cpattern * imp_expr) list;
}

type imp_node = {
  i_name : ident;
  i_inputs : ident list;
  i_outputs : ident list;
  i_inits : imp_inits;
  i_step_fun : imp_step;
}
