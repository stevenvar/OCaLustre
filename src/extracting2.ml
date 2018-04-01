open Parsetree
open Asttypes
open Longident
open Imperative_ast2
open Ast_helper
open Tools

 let rec tocaml_cond_list cl =
    match cl with
    | [] -> [%expr () ]
    | [(b,x)] -> let b = Ast_convenience.constr (string_of_bool b) [] in
      let s = Ast_convenience.evar x in 
      [%expr [%e s] = [%e b] ]
    | (hb,hx)::t ->
      let e1 = tocaml_cond_list [(hb,hx)] in
      let e2 = tocaml_cond_list t in
      [%expr [%e e1] && [%e e2]]

let rec tocaml_expression e =
  let open Parsing_ast in
  match e with
  | IValue (String s) -> Ast_convenience.str s
  | IValue (Nil) -> [%expr Obj.magic () ]
  | IValue (Enum s) -> Ast_convenience.constr s []
  | IValue (Integer i) -> Ast_convenience.int i
  | IValue (Float f) -> Ast_convenience.float f
  | IValue (Bool true) -> Ast_convenience.constr "true" []
  | IValue (Bool false) ->  Ast_convenience.constr "false" []
  | IETuple t -> Ast_convenience.tuple (List.map (fun i -> tocaml_expression i) t)
  | IVariable i -> Ast_convenience.evar i
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
    [%expr [%e (Exp.ident (lid_of_ident (id^n^"_app")))] [%e e' ]]
  | ICondact(l,e) ->
    let e' = tocaml_expression e in
    (* let b' = Ast_convenience.constr (string_of_bool b) [] in *)
    (* let i = Ast_convenience.evar i in *)
    let cl = tocaml_cond_list l in
    [%expr if [%e cl] then [%e e'] else Obj.magic ()]
  | IApplication_init (id,e) ->
    let e' = tocaml_expression e in
    [%expr [%e (Exp.ident (lid_of_ident (id)))] [%e e']]
  | IAlternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1)]]
        [%expr  [%e (tocaml_expression e2)] ]
        (Some ( [%expr  [%e tocaml_expression e3 ]]))
    ]
    ]
  | IUnit -> [%expr ()]
  | IConstr _ -> [%expr ()]
  | ICall e ->
    [%expr [%e e ]]
  | _ -> failwith "todo"

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    let x = e.i_pattern in
    let ppat = pat_of_pattern x in
    let pexpr = tocaml_expression e.i_expression in
    [%expr let [%p ppat] = [%e pexpr] in [%e acc ]]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc (List.rev el)

let tocaml_updates l acc =
  let rec aux { i_condition = c; i_pattern = p; i_expression = e } acc =
    let cond = tocaml_cond_list c in
    let p = expr_of_pattern p in
    let e = tocaml_expression e in
    match c with
    | [] -> [%expr [%e p] := [%e e]; [%e acc]]
    | _ -> [%expr if [%e cond] then [%e p] := [%e e]; [%e acc] ]
  in
  List.fold_left (fun acc u -> aux u acc) acc l

let tocaml_step inode =
  let outputs = expr_of_pattern inode.i_outputs in
  let equations = tocaml_eq_list inode.i_step.i_step_equations (tocaml_updates inode.i_step.i_step_updates outputs) in
  [%expr fun [%p pat_of_pattern inode.i_inputs] -> [%e equations]]

let tocaml_inits il acc =
  let aux {i_pattern = p ; i_expression = e}  acc =
      [%expr let [%p pat_of_pattern p] =
               [%e tocaml_expression e] in [%e acc]]
  in
  List.fold_left (fun acc i -> aux i acc) acc il

let tocaml_init i acc =
  tocaml_inits (i.i_init_apps@i.i_init_fby) acc


let tocaml_main inode =
  let name = expr_of_pattern inode.i_name in
  let input_fun = suffix_pattern ~suf:"_inputs" inode.i_name in
  let input_fun = expr_of_pattern input_fun in
  let output_fun = suffix_pattern ~suf:"_outputs" inode.i_name in
  let output_fun = expr_of_pattern output_fun in
  [%stri
    let () =
      let open IO in
           let main = [%e name] ()  in
           while true do
           let [%p pat_of_pattern inode.i_inputs ] = [%e input_fun] () in
           let [%p pat_of_pattern inode.i_outputs ] =
             main [%e expr_of_pattern inode.i_inputs] in
           [%e output_fun] [%e expr_of_pattern inode.i_outputs ]
         done ]

let tocaml_node inode =
  let name = stringloc_of_ident (get_ident inode.i_name) in
  let step = tocaml_step inode in
  [%stri let [%p Pat.var name] =
           fun () ->
             [%e tocaml_init inode.i_init step]
  ]
