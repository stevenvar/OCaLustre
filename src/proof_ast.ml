
open Parsing_ast
open Clocking_ast

type app_inits = s_equation list
and s_expr =
  | S_Value of value
  | S_Constr of string
  | S_Variable of ident
  | S_Application of ident * int * s_expr
  | S_Application_init of ident * s_expr
  | S_Call of ident * s_expr
  | S_Ref of ident
  | S_RefDef of s_expr
  | S_InfixOp of imp_infop * s_expr * s_expr
  | S_PrefixOp of imp_preop * s_expr
  | S_Alternative of s_expr * s_expr * s_expr
  | S_ExpTuple of s_expr list
  | S_Unit
and
  imp_infop =
  | S_Equals
  | S_Diff
  | S_Plus
  | S_Minus
  | S_Times
  | S_Div
  | S_Plusf
  | S_Minusf
  | S_Divf
  | S_Timesf
  | S_Inf
  | S_Infe
  | S_Sup
  | S_Supe
  | S_Or
  | S_And
  | S_Mod

and
  imp_preop =
  | S_Not
  | S_Neg
  | S_Negf

and s_equation =  {
  s_pattern : pattern;
  s_expression : s_expr;
}


and s_condition = s_expr option

type s_init = {
  si_name : pattern;
  si_pre : s_condition;
  si_post : s_condition;
  si_inv : s_condition;
  si_inputs : pattern;
  si_outputs : pattern;
  si_equations : s_equation list;
}

type s_step = {
  ss_name : pattern;
  ss_pre : s_condition;
  ss_post : s_condition;
  ss_inv : s_condition;
  ss_inputs : pattern;
  ss_outputs : pattern;
  ss_equations : s_equation list;
}

type s_node = {
  s_pre : s_condition;
  s_post : s_condition;
  s_inv : s_condition;
  s_name : pattern;
  s_inputs : pattern;
  s_outputs : pattern;
  s_apps_init : app_inits;
  s_init_fun : s_init;
  s_step_fun : s_step;
}
