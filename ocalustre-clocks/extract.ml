open Parsetree
open Asttypes
open Longident
open Ast
open Compiling
open Ast_helper

let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i.content^suffix);
    loc = i.loc 
  }

let rec tocaml_expression e   =

    match e with
    | IValue (Integer i) -> Exp.constant (Pconst_integer (string_of_int i,None))  
    | ITuple t -> Exp.tuple (List.map tocaml_expression t)
    | IVariable i -> [%expr  [%e Exp.ident (lid_of_ident i) ] ]
    | IRef i -> [%expr ![%e Exp.ident (lid_of_ident ~prefix:"pre_" i) ]  ]
    | IInfixOp (IDiff,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] <> [%e tocaml_expression e2 ]]
    | IInfixOp (IEquals,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] = [%e tocaml_expression e2 ]]
    | IInfixOp (IPlus,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] + [%e tocaml_expression e2 ]]
    | IInfixOp (IMinus,e1,e2) ->
       [%expr [%e tocaml_expression e1 ] - [%e tocaml_expression e2 ]]
    | IInfixOp (ITimes,e1,e2) ->
       [%expr [%e tocaml_expression e1 ] * [%e tocaml_expression e2 ]]
    | IInfixOp (IDiv,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] / [%e tocaml_expression e2 ]]
    | IAlternative (e1,e2,e3) -> 
      [%expr [%e Exp.ifthenelse
                [%expr [%e (tocaml_expression e1) ]]
                [%expr  [%e (tocaml_expression e2) ] ] 
                (Some ( [%expr  [%e tocaml_expression e3 ] ] ))  
                ] 
      ]
    | IUnit -> [%expr ()]
    | _ -> [%expr ()]
    


let stringloc_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = prefix^i.content^suffix;
    loc = i.loc; 
  }

let tocaml_updates node outs =
  let aux (p,e) acc =
    [%expr [%e Exp.ident (lid_of_ident ~prefix:"pre_" p) ] := ([%e tocaml_expression e]) ;
           [%e acc ]]
  in
  List.fold_left (fun acc u -> aux u acc) outs node.i_step_fun.i_updates 


let tocaml_outputs node =
  let aux ol =
    List.map (fun o -> Exp.ident (lid_of_ident o)) ol
  in 
  match node.i_outputs with
  | [] -> [%expr () ]
  | [x] -> [%expr [%e Exp.ident (lid_of_ident x) ]]
  | _ -> [%expr [%e Exp.tuple (aux node.i_outputs) ] ]

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    let x = e.i_pattern in 
    let ppat = stringloc_of_ident x in
    let pexpr = tocaml_expression e.i_expression in 
    [%expr let [%p Ast_helper.Pat.var ppat ] = ( [%e pexpr ] ) in  [%e acc ]]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc el
  
let tocaml_inits inits acc =
  let aux (p,e) acc =
     [%expr let [%p Pat.var (stringloc_of_ident ~prefix:"pre_" p)] =
              ref ([%e tocaml_expression e]) in [%e acc] ]
  in
  List.fold_left (fun acc i -> aux i acc) acc inits

let tocaml_inputs node pname acc =
  let aux il =
    List.map (fun i -> Pat.var (stringloc_of_ident i)) il
  in 
  let inputs = node.i_inputs in 
  match inputs with
  | [] ->
    [%expr let [%p Pat.var pname ] =
             fun () -> [%e acc ]
           in
           [%e Exp.ident (lid_of_ident ~suffix:"_step" node.i_name) ]]
  | [x] ->
    [%expr let [%p Pat.var pname] =
             fun [%p Pat.var (stringloc_of_ident x)] -> [%e acc ]
           in
           [%e Exp.ident (lid_of_ident ~suffix:"_step" node.i_name)]]
  | _ ->
    [%expr let [%p Pat.var pname] =
             fun [%p Pat.tuple (aux node.i_inputs) ] -> [%e acc]
           in
           [%e Exp.ident (lid_of_ident ~suffix:"_step" node.i_name)]]



let tocaml_step node =
  let pname = stringloc_of_ident ~suffix:"_step" node.i_name in
  let outs = tocaml_outputs node in
  let ups = tocaml_updates node outs in
  let eqs = tocaml_eq_list (List.rev node.i_step_fun.i_equations) ups in 
  tocaml_inputs node pname eqs
  
let extract_node inode =
  let name = stringloc_of_ident inode.i_name in
  let inits = inode.i_inits in

  [%stri let [%p Pat.var name] =
           fun () ->
             [%e tocaml_inits inits (tocaml_step inode) ]
  ]
