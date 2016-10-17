
open Parsetree
open Parsing_ast
open Clocking_ast

type app_inits = imp_equation list
and init = imp_equation
and imp_inits = init list
and imp_expr =
  | IValue of constant
  | IConstr of string
  | IVariable of ident
  | IApplication of ident * int * imp_expr
  | IApplication_init of ident * imp_expr 
  | ICall of Parsetree.expression 
  | IRef of ident
  | IRefDef of imp_expr
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
  | IInf
  | IInfe
  | ISup
  | ISupe
    
and
  imp_preop =
  | INot
  | INeg
  | INegf

and imp_equation =  {
  i_pattern : pattern;
  i_expression : imp_expr;
}

type imp_step = {
  i_equations : imp_equation list;
  i_updates : imp_equation list;
}

type imp_node = {
  i_name : ident;
  i_inputs : pattern;
  i_outputs : pattern;
  i_inits : imp_inits;
  i_app_inits : imp_inits;
  i_fby_inits : imp_inits; 
  i_step_fun : imp_step;
}
