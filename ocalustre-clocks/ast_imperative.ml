open Parsetree
open Asttypes
open Longident
open Ast
open Clocked_ast


type app_inits = (cpattern * imp_expr) list
and imp_inits = (cpattern * imp_expr) list 
and imp_expr =
  | IValue of Ast.constant
  | IVariable of stream
  | ITuple of imp_expr list 
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IUnit
and imp_infop =
  | IEquals
  | IDiff
  | IPlus
  | IMinus
  | ITimes
  | IDiv
and imp_preop =
  | INot

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
  i_app_inits : app_inits; 
  i_inits : imp_inits;
  i_step_fun : imp_step;
}

let counter =
  let count = ref (0) in
  fun () -> incr count; !count


let ident_to_stringloc i =
  {
    txt = i.content;
    loc = i.loc; 
  }

let ident_to_lid i =
  {
    txt = Lident i.content;
    loc = i.loc;
  }

let ident_to_stringloc_suffix i suffix =
  {
    txt = i.content^suffix;
    loc = i.loc; 
  }
 let ident_to_lid_suffix i suffix =
  {
    txt = Lident (i.content^suffix);
    loc = i.loc;
  }

let clock_to_ident c =
  match c with
  | On (ck,x) -> x
  | _ -> failwith "cannot do current of global clock"           

let get_ident (e,c) =
  match e with
  | C_Variable v -> v
  | _ -> assert false 

let rec compile_expression (exp,clock) =
  let compile_preop op =
    match op with
    | Not -> INot
  in
  let compile_infop op =
    match op with
    | Equals -> IEquals
    | Plus -> IPlus
    | Minus -> IMinus
    | Times -> ITimes
    | Div -> IDiv
    | Diff -> IDiff

  in
  let rec compile_expression_without_clock exp = 
    match exp with
      | CValue v -> IValue v
      | CVariable v -> IVariable v
      | CInfixOp (op,e1,e2) ->
        IInfixOp(compile_infop op,
                 compile_expression_without_clock (fst e1),
                 compile_expression_without_clock (fst e2))
      | CPrefixOp (op, e) ->
        IPrefixOp (compile_preop op, compile_expression_without_clock (fst e))
      | CAlternative (e1,e2,e3) ->
        IAlternative (compile_expression_without_clock (fst e1),
                      compile_expression_without_clock (fst e2),
                      compile_expression_without_clock (fst e3))
      | CUnit -> IUnit
      | CFby (v,e) -> compile_expression_without_clock (fst e) 
  in
  match clock with 
  | Base  ->
    begin
      match exp with
      | C_Value v -> IValue v
      | C_Variable v -> IVariable v
      | C_InfixOp (op,e1,e2) ->
        IInfixOp(compile_infop op,
                 compile_expression_without_clock (fst e1),
                 compile_expression_without_clock (fst e2))
      | C_PrefixOp (op, e) ->
        IPrefixOp (compile_preop op, compile_expression_without_clock (fst e))
      | C_Alternative (e1,e2,e3) ->
        IAlternative (compile_expression_without_clock (fst e1),
                      compile_expression_without_clock (fst e2),
                      compile_expression_without_clock (fst e3))
      | C_Unit -> IUnit
      | C_Fby (v,e) -> compile_expression_without_clock (fst e) 
    end 
  | On (ck,x) -> (compile_expression_without_clock exp)

let generate_inits (node:c_node) =
  let generate_from_e equation l =
    match fst equation.c_expression with
    | C_Fby (v,e) -> (equation.c_pattern, IValue v)::l
    | _ -> l
  in 
  let generate_from_el (equations:c_equation list) = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l) [] equations in
      generate_from_el node.c_equations


let generate_updates node =
  let generate_from_e equation l =
    match fst equation.c_expression with
    | C_Fby(v,e) ->
      (equation.c_pattern, (compile_expression e))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [ ] equations
  in
  (* list of couples (string, value) *) 
  generate_from_el node.c_equations       

let compile_io l =
  List.map (fun (io,c) -> io) l
    
let compile_equation e = {
  i_pattern = e.c_pattern;
  i_expression = compile_expression e.c_expression;
}

let remove_forbidden_op_list el =
  let remove_forbidden_op e l =
    match fst e.c_expression with
    | _ -> e::l
  in List.fold_left (fun l e -> let l = remove_forbidden_op e l in l ) [] el  

let compile_node (node:c_node) =
  {
    i_name = node.c_name;
    i_inputs = compile_io node.c_inputs;
    i_outputs =compile_io node.c_outputs;
    i_app_inits = [];
    i_inits = (generate_inits node);
    i_step_fun =
      {
        i_equations =
          List.map
            (compile_equation)
            (remove_forbidden_op_list node.c_equations);
        i_updates = ( generate_updates node);
      };
  }


let rec tocaml_expression e   =
  Ast_helper.( 
    match e with
    | IValue (Integer i) -> Exp.constant (Const_int i)  
    | ITuple t -> Exp.tuple (List.map tocaml_expression t)
    | IVariable (i,c) -> [%expr  [%e Exp.ident (ident_to_lid i) ] ]
    | IRef i -> [%expr Option.get ![%e Exp.ident (ident_to_lid i) ]  ]
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
    )

let tocaml_app_inits il acc =
  let tocaml_app_init (s,e) acc =
    match s with
    | (p,c) ->
        [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc p)] =
                 [%e tocaml_expression e ] in [%e acc ] ]
  in List.fold_left (fun l i -> tocaml_app_init i l ) acc il 

let tocaml_inits il acc =
  let tocaml_init (s,e) acc =
    match s with
    | (p,c) ->
      begin match e with
      |  x ->
        [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc p)] =
                 ref ([%e tocaml_expression x ]) in [%e acc ] ]
      end
  in List.fold_left (fun l i -> tocaml_init i l ) acc il 


let tocaml_inputs il =
  List.map (fun i -> Ast_helper.Pat.var (ident_to_stringloc i) ) il
    
let tocaml_outputs ol =
  List.map (fun o -> Ast_helper.Exp.ident (ident_to_lid o) ) ol 

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    match e.i_pattern with
    | (x,c) -> 
      let ppat = ident_to_stringloc x in
      let pexpr = tocaml_expression e.i_expression in 
      [%expr let [%p Ast_helper.Pat.var ppat ] = ( [%e pexpr ] ) in  [%e acc ]]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc el

let tocaml_updates ul acc =
  let tocaml_update (s,e) acc =
    match s with
    | (p,c) -> 
      begin match e with
        | (IValue v ) ->
          [%expr [%e Ast_helper.Exp.ident (ident_to_lid p)] :=
                   (  [%e tocaml_expression (IValue v) ]); [%e acc ] ] 
        | x ->
          [%expr [%e Ast_helper.Exp.ident (ident_to_lid p)] :=
                   ( [%e tocaml_expression x ]); [%e acc ] ]
      end
  in
  List.fold_left (fun l u -> tocaml_update u l) acc ul
    

let tocaml_step_fun node =
  let pname = ident_to_stringloc_suffix node.i_name "_step" in
  let out =
    match node.i_outputs with
    | [] -> [%expr () ]
    | [x] -> [%expr [%e Ast_helper.Exp.ident (ident_to_lid x)]]
    | _ -> [%expr [%e
                     Ast_helper.Exp.tuple (tocaml_outputs node.i_outputs)
                  ]]
  in
  let ups = [%expr [%e tocaml_updates node.i_step_fun.i_updates out ]] in
  match node.i_inputs with
  | [] -> [%expr let [%p Ast_helper.Pat.var pname ] =
             fun () ->
               ([%e tocaml_eq_list node.i_step_fun.i_equations
                      ups
               ])
           in
           [%e
             Ast_helper.Exp.ident
               (ident_to_lid_suffix node.i_name "_step")
           ]
    ]
  | [x] ->
    [%expr let [%p Ast_helper.Pat.var pname ] =
             fun [%p Ast_helper.Pat.var (ident_to_stringloc x) ] ->
               ([%e tocaml_eq_list node.i_step_fun.i_equations
                      ups
               ])
           in
           [%e
             Ast_helper.Exp.ident
               (ident_to_lid_suffix node.i_name "_step")
           ]
    ]
  | _ -> 
    [%expr let [%p Ast_helper.Pat.var pname ] =
             fun [%p Ast_helper.Pat.tuple (tocaml_inputs node.i_inputs) ] ->
               ([%e tocaml_eq_list node.i_step_fun.i_equations
                      ups
               ])
           in

           [%e
             Ast_helper.Exp.ident
               (ident_to_lid_suffix node.i_name "_step")
           ]
    ] 

let tocaml_node inode =
  let pname = ident_to_stringloc inode.i_name in
  let app_inits = inode.i_app_inits in
  let inits = inode.i_inits in

  [%stri let [%p Ast_helper.Pat.var pname ]  =
           fun () -> [%e
             tocaml_app_inits
               app_inits
               (tocaml_inits
                  inits
                  (tocaml_step_fun inode)
               )
           ]
  ]

