open Parsetree
open Asttypes
open Longident
open Parsing_ast
open Imperative_ast
open Clocking_ast
open Compiling
open Ast_helper

let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i^suffix);
    loc = Location.none
  }


let rec tocaml_expression e =
  match e with
  | IValue (Nil) -> [%expr Obj.magic () ]
  | IValue (Enum s) ->  Exp.construct {txt = Lident s; loc = Location.none} None
  | IValue (Integer i) -> Exp.constant (Pconst_integer (string_of_int i,None))
  | IValue (Float f) -> Exp.constant (Pconst_float (string_of_float f,None))
  | IValue (Bool true) ->
    Exp.construct {txt= Lident "true" ; loc = Location.none } None
  | IValue (Bool false) ->
    Exp.construct {txt= Lident "false" ; loc = Location.none } None
  | IETuple t -> Exp.tuple (List.map (fun i -> tocaml_expression i) t)
  | IVariable i -> [%expr  [%e Exp.ident (lid_of_ident i) ] ]
  | IRef i -> [%expr ![%e Exp.ident (lid_of_ident i) ]  ]
  | IRefDef e -> [%expr ref [%e tocaml_expression e ] ]
  | IPrefixOp (INot, e) -> [%expr not [%e tocaml_expression e ] ]
  | IPrefixOp (INeg, e) -> [%expr ~- [%e tocaml_expression e ] ]
  | IPrefixOp (INegf, e) -> [%expr ~-. [%e tocaml_expression e ] ]
  | IInfixOp (IDiff,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <> [%e tocaml_expression e2 ]]
  | IInfixOp (IEquals,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] = [%e tocaml_expression e2 ]]
  | IInfixOp (IMod,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] mod [%e tocaml_expression e2 ]]
  | IInfixOp (IPlus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] + [%e tocaml_expression e2 ]]
  | IInfixOp (IMinus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] - [%e tocaml_expression e2 ]]
  | IInfixOp (ITimes,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] * [%e tocaml_expression e2 ]]
  | IInfixOp (IDiv,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] / [%e tocaml_expression e2 ]]
  | IInfixOp (IPlusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] +. [%e tocaml_expression e2 ]]
  | IInfixOp (IMinusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] -. [%e tocaml_expression e2 ]]
  | IInfixOp (ITimesf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] *. [%e tocaml_expression e2 ]]
  | IInfixOp (IDivf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ]  /. [%e tocaml_expression e2 ]]
  | IInfixOp (IInf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] < [%e tocaml_expression e2 ]]
  | IInfixOp (IInfe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <= [%e tocaml_expression e2 ]]
  | IInfixOp (ISup,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] > [%e tocaml_expression e2 ]]
  | IInfixOp (ISupe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] >= [%e tocaml_expression e2 ]]
  | IInfixOp (IOr,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] || [%e tocaml_expression e2 ]]
  | IInfixOp (IAnd,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] && [%e tocaml_expression e2 ]]
  | IApplication (id, num, e) ->
    let e' = tocaml_expression e in
    let n = string_of_int num in
    [%expr [%e (Exp.ident (lid_of_ident (id^n^"_step")))] [%e e' ]]
  | IApplication_init (id,e) ->
    let e' = tocaml_expression e in
    [%expr [%e (Exp.ident (lid_of_ident (id)))] [%e e' ]]
  | IAlternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1) ]]
        [%expr  [%e (tocaml_expression e2) ] ]
        (Some ( [%expr  [%e tocaml_expression e3 ] ] ))
    ]
    ]
  | IUnit -> [%expr ()]
  | IConstr _ -> [%expr ()]
  | ICall e ->
    [%expr [%e e ]]
