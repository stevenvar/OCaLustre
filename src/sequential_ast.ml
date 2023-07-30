open Parsing_ast
open Clocking_ast

type app_inits = s_equation list
and init = s_equation
and s_inits = init list
and s_expr =
  | S_Magic
  | S_Value of value
  | S_Constr of string
  | S_Variable of ident
  | S_Application of ident * int * s_expression list
  | S_Application_init of ident * int * s_expression list
  | S_Call of (ident*s_expression list)
  | S_Ref of ident
  | S_RefDef of s_expression
  | S_InfixOp of s_infop * s_expression * s_expression
  | S_PrefixOp of s_preop * s_expression
  | S_Alternative of s_expression * s_expression * s_expression
  | S_ETuple of s_expression list
  | S_Field of s_expression * ident
  | S_List of s_expression list
  | S_Unit
and
  s_infop =
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
  s_preop =
  | S_Not
  | S_Neg
  | S_Negf

and s_expression = {
  s_e_desc : s_expr;
  s_e_loc : Location.t
}


and s_equation =  {
  s_pattern : pattern;
  s_expression : s_expression;
}

type s_state = {
  pres : ident list;
  calls : ident list;
  outs : ident list;}

type s_fun = {
  s_name : pattern;
  s_inputs : ident list;
  s_outputs : ident list;
  s_state : s_state;
  s_eqs : s_equation list;
}

type s_typ = {
  s_num : int;
  s_name : pattern;
  s_attr : ident list;
}

type s_node = {
  s_name : pattern;
  s_type : s_typ;
  s_zero : s_fun;
  s_next : s_fun;
}
