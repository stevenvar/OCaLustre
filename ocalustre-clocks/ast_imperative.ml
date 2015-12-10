open Parsetree
open Asttypes
open Longident
open Ast 
open Ast_clock

type app_inits = (c_pattern * imp_expr) list
and  imp_inits = (c_pattern * imp_expr option) list 
and
  imp_expr =
  | IValue of Ast.constant
  | IVariable of ident
  | ITuple of imp_expr list 
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IApplication of ident * imp_expr list
  | IApplication_init of ident * imp_expr list
  | ICall of Parsetree.expression
  | IWhen of imp_expr * ident 
  | IUnit
and
  imp_infop =
  | IEquals
  | IDiff
  | IGreat
  | IGreate
  | ILess
  | ILesse
  | IPlus
  | IMinus
  | ITimes
  | IDiv
  | IPlusf
  | IMinusf
  | ITimesf
  | IDivf
  | IAnd
  | IOr
and
  imp_preop =
  | IPre
  | INot

type imp_equation =  {
  i_pattern : c_pattern;
  i_expression : imp_expr;
} 

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (c_pattern * imp_expr option) list;
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
 
let rec compile_expression (exp,clock) =
  let compile_preop op =
    match op with
    | Pre -> IPre
    | Not -> INot
  in
  let compile_infop op =
    match op with
    | Equals -> IEquals
    | Plus -> IPlus
    | Minus -> IMinus
    | Times -> ITimes
    | Div -> IDiv
    | Plusf -> IPlusf
    | Minusf -> IMinusf
    | Timesf -> ITimesf
    | Divf -> IDivf
    | Lesse -> ILesse
    | Great -> IGreat
    | Greate -> IGreate
    | Less -> ILess
    | Diff -> IDiff
    | And -> IAnd
    | Or -> IOr
    | _ -> failwith "forbidden operator"
  in
  let rec compile_expression_without_clock exp = 
    match exp with
      | C_Value v -> IValue v
      | C_Tuple t -> ITuple (List.map (compile_expression_without_clock) (List.map fst t))
      | C_Variable v -> IVariable v
      | C_Ref v -> IRef v
      | C_InfixOp (op,e1,e2) -> IInfixOp(compile_infop op,
                                    compile_expression_without_clock (fst e1),
                                       compile_expression_without_clock (fst e2))
      | C_PrefixOp (op, e) -> IPrefixOp (compile_preop op, compile_expression_without_clock (fst e))
      | C_Alternative (e1,e2,e3) ->  IAlternative (compile_expression_without_clock (fst e1),
                                                 compile_expression_without_clock (fst e2),
                                                 compile_expression_without_clock (fst e3))
      | C_Application (id, el) -> IApplication (id,
                                              List.map (compile_expression_without_clock) (List.map fst el) )
      | C_Application_init (id, el) -> IApplication_init (id,
                                              List.map (compile_expression_without_clock) (List.map fst el))
      | C_Call e -> ICall e 
      | C_When (e,i) -> compile_expression_without_clock (fst e)
      | C_Unit -> IUnit
    in
  match clock with 
  | Constant | Global -> 
    begin
      match exp with
      | C_Value v -> IValue v
      | C_Tuple t -> ITuple (List.map (compile_expression_without_clock) (List.map fst t))
      | C_Variable v -> IVariable v
      | C_Ref v -> IRef v
      | C_InfixOp (op,e1,e2) -> IInfixOp(compile_infop op,
                                    compile_expression_without_clock (fst e1),
                                       compile_expression_without_clock (fst e2))
      | C_PrefixOp (op, e) -> IPrefixOp (compile_preop op, compile_expression_without_clock (fst e))
      | C_Alternative (e1,e2,e3) ->  IAlternative (compile_expression_without_clock (fst e1),
                                                 compile_expression_without_clock (fst e2),
                                                 compile_expression_without_clock (fst e3))
      | C_Application (id, el) -> IApplication (id,
                                              List.map (compile_expression_without_clock) (List.map fst el) )
      | C_Application_init (id, el) -> IApplication_init (id,
                                              List.map (compile_expression_without_clock) (List.map fst el))
      | C_Call e -> ICall e 
      | C_When (e,i) -> compile_expression_without_clock (fst e)
      | C_Unit -> IUnit
    end 
  | Clock x -> IWhen(compile_expression_without_clock exp, {loc=Location.none ; content=x})

let generate_inits (node:c_node) =
  let generate_from_e equation l =
    match fst equation.c_expression with
    | C_PrefixOp (Pre, x) -> (equation.c_pattern,None)::l
    | C_InfixOp (Arrow, x, y) -> (equation.c_pattern,
                                Some (compile_expression x))::l
    | _ -> l
  in 
  let generate_from_el (equations:c_equation list) = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [] equations in
      generate_from_el node.c_equations

let generate_app_inits node =
  let generate_from_e equation l =
    match fst equation.c_expression with
    | C_Application_init (i, li) -> (equation.c_pattern, IApplication_init (i, (List.map compile_expression li)))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [] equations
      in
  (* list of couples (string, value) *) 
      generate_from_el node.c_equations

let generate_updates node =
  let generate_from_e equation l =
    match fst equation.c_expression with
    | C_PrefixOp (Pre, x) ->
      (equation.c_pattern, Some (compile_expression x) )::l
    | C_InfixOp (Arrow, x, y) -> (equation.c_pattern, Some (compile_expression y))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [ ] equations
  in
  (* list of couples (string, value) *) 
  generate_from_el node.c_equations       

let compile_io l =
  List.map (fun io -> io) l
    
let compile_equation e = {
  i_pattern = e.c_pattern;
  i_expression = compile_expression e.c_expression;
}

let remove_forbidden_op_list el =
  let remove_forbidden_op e l =
    match fst e.c_expression with
    | C_PrefixOp (Pre, x) -> l
    | C_InfixOp (Arrow,x,y) -> l
    | C_Application_init (i,li) -> l
    | _ -> e::l
  in List.fold_left (fun l e -> let l = remove_forbidden_op e l in l ) [] el  

let compile_node (node:c_node) =
  {
    i_name = node.c_name;
    i_inputs = compile_io node.c_inputs;
    i_outputs =compile_io node.c_outputs;
    i_app_inits = (generate_app_inits node);
    i_inits = (generate_inits node);
    i_step_fun =
      {
        i_equations =
          List.map (compile_equation) (remove_forbidden_op_list node.c_equations);
        i_updates = ( generate_updates node);
      };
  }


let rec tocaml_expression e   =
  Ast_helper.( 
    match e with
    | IValue (Integer i) -> [%expr Value [%e Exp.constant (Const_int i) ] ] 
    | IValue (Float f) -> Exp.constant (Const_float (string_of_float f))
    | IValue (Bool true) -> Exp.construct {txt= Lident "true" ; loc = Location.none } None
    | IValue (Bool false) -> Exp.construct {txt=Lident "false" ; loc = Location.none }  None
    | ITuple t -> Exp.tuple (List.map tocaml_expression t)
    | IVariable i -> [%expr  [%e Exp.ident (ident_to_lid i) ] ]
    | IRef i -> [%expr ![%e Exp.ident (ident_to_lid i) ]  ]
    | IWhen (e1,i) ->
      [%expr [%e Exp.ifthenelse
                ([%expr Flow.get [%e Exp.ident (ident_to_lid i) ]])
                ( [%expr [%e (tocaml_expression e1) ] ] )  
                (Some [%expr None ])  ] ]
    | IInfixOp (IDiff,e1,e2) ->
      [%expr Flow.map2 (<>) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (ILess,e1,e2) ->
      [%expr Flow.map2 (<) [%e tocaml_expression e1 ][%e tocaml_expression e2 ]]
    | IInfixOp (IGreat,e1,e2) ->
      [%expr Flow.map2 (>) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IGreate,e1,e2) ->
      [%expr Flow.map2 (>=) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]] 
    | IInfixOp (ILesse,e1,e2) ->
      [%expr Flow.map2 (<) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IEquals,e1,e2) ->
      [%expr Flow.map2 (=) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]

    | IInfixOp (IPlus,e1,e2) ->
      [%expr  Flow.map2 (+) ([%e tocaml_expression e1 ]) ([%e tocaml_expression e2 ]) ]
    | IInfixOp (IMinus,e1,e2) ->
       [%expr Flow.map2 (-) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (ITimes,e1,e2) ->
       [%expr Flow.map2 ( * ) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IDiv,e1,e2) ->
      [%expr Flow.map2 (/) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IPlusf,e1,e2) ->
      [%expr Flow.map2 (+.) [%e tocaml_expression e1 ]  [%e tocaml_expression e2 ]]
    | IInfixOp (IMinusf,e1,e2) ->
       [%expr Flow.map2 (-.) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (ITimesf,e1,e2) ->
       [%expr Flow.map2 ( *.) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IDivf,e1,e2) ->
      [%expr Flow.map2 (/.)[%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IPrefixOp (INot, e) -> [%expr Flow.map (not) [%e tocaml_expression e] ]
    | IInfixOp (IAnd,e1,e2) ->
       [%expr Flow.map2 (&&) [%e tocaml_expression e1 ] [%e tocaml_expression e2 ]]
    | IInfixOp (IOr,e1,e2) ->
       [%expr Flow.map2 (||) [%e tocaml_expression e1 ]  [%e tocaml_expression e2 ]]
    | IAlternative (e1,e2,e3) -> 
      [%expr [%e Exp.ifthenelse
                [%expr Flow.get [%e (tocaml_expression e1) ]]
                [%expr  [%e (tocaml_expression e2) ] ] 
                (Some ( [%expr  [%e tocaml_expression e3 ] ] ))  
                ] 
      ]
    | IApplication (id, el) ->
      let listexp = match el with
        | [] ->  [("", [%expr () ] )]
        | [x] ->  [("", tocaml_expression x)]
        | _ -> [("",Exp.tuple (List.map (fun x -> [%expr [%e tocaml_expression x ]]) el))] in 
      Exp.apply
        (Exp.ident (ident_to_lid id)) listexp
    |IApplication_init (id, el) ->
      let listexp = match el with
        | [] ->  [("", [%expr () ] )]
        | [x] ->  [("", tocaml_expression x)]
        | _ -> [("",Exp.tuple (List.map (fun x -> [%expr [%e tocaml_expression x ]]) el))] in 
      Exp.apply
        (Exp.ident (ident_to_lid id)) listexp
    | IUnit -> [%expr ()]
    | ICall e -> e
    | _ -> [%expr ()]
    )
let tocaml_app_inits il acc =
  let tocaml_app_init (s,e) acc =
    match s with
    | C_Simple (p,c) ->
        [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc p)] =
                 [%e tocaml_expression e ] in [%e acc ] ]
    | C_List t ->
      let stringloclist = List.map (ident_to_stringloc) (List.map fst t) in
      let patlist = List.map (fun x -> Ast_helper.Pat.var x) stringloclist in
        [%expr let [%p Ast_helper.Pat.tuple patlist] =
                 [%e tocaml_expression e ] in [%e acc ] ]
      in List.fold_left (fun l i -> tocaml_app_init i l ) acc il 

let tocaml_inits il acc =
  let tocaml_init (s,e) acc =
    match s with
    | C_Simple (p,c) ->
      begin match e with
      | None ->
        [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc p)] =
                 ref Nil in [%e acc ] ]  
      | Some x ->
        [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc p)] =
                 ref (Value [%e tocaml_expression x ]) in [%e acc ] ]
      end
    | C_List t ->
      let stringloclist = List.map (ident_to_stringloc) (List.map fst t) in
      let patlist = List.map (fun x -> Ast_helper.Pat.var x) stringloclist in
      begin match e with
      | None ->
        [%expr let [%p Ast_helper.Pat.tuple patlist ] =
                 ref Nil in [%e acc ] ]  
      | Some x ->
        [%expr let [%p Ast_helper.Pat.tuple patlist] =
                 ref (Value [%e tocaml_expression x ]) in [%e acc ] ]
      end
      
  in List.fold_left (fun l i -> tocaml_init i l ) acc il 

let tocaml_inputs il =
  List.map (fun i -> Ast_helper.Pat.var (ident_to_stringloc i) ) il
    
let tocaml_outputs ol =
  List.map (fun o -> Ast_helper.Exp.ident (ident_to_lid o) ) ol 

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    match e.i_pattern with
    | C_Simple (x,c) -> 
      let ppat = ident_to_stringloc x in
      let pexpr = tocaml_expression e.i_expression in 
      [%expr let [%p Ast_helper.Pat.var ppat ] = ( [%e pexpr ] ) in  [%e acc ] ]
    | C_List t ->
      let lpat = List.map ident_to_stringloc (List.map fst t) in
      let lastpat = List.map (fun x -> Ast_helper.Pat.var x ) lpat in 
      let pexpr = tocaml_expression e.i_expression in 
      [%expr let [%p Ast_helper.Pat.tuple lastpat ] = ( [%e pexpr ] ) in  [%e acc ] ]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc el

let tocaml_updates ul acc =
  let tocaml_update (s,e) acc =
    match s with
    | C_Simple (p,c) -> 
      begin match e with
        | None ->
          [%expr [%p Ast_helper.Exp.ident (ident_to_lid p)] :=
                   Init ; [%e acc ] ] 
        | Some (IValue v ) ->
          [%expr [%e Ast_helper.Exp.ident (ident_to_lid p)] :=
                   ( Value [%e tocaml_expression (IValue v) ]); [%e acc ] ] 
        | Some x ->
          [%expr [%e Ast_helper.Exp.ident (ident_to_lid p)] :=
                   ([%e tocaml_expression x ]); [%e acc ] ]
      end
    | C_List t ->
      let lidlist = List.map ident_to_lid (List.map fst t) in
      let explist = List.map (fun x -> Ast_helper.Exp.ident x) lidlist in
       begin match e with
        | None ->
          [%expr [%p Ast_helper.Exp.tuple explist] :=
                   Init ; [%e acc ] ]  
        | Some x ->
          [%expr [%e Ast_helper.Exp.tuple explist] :=
                   ([%e tocaml_expression x ]) ; [%e acc ] ]
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
  
  [%stri let [%p Ast_helper.Pat.var pname ]  = fun () ->
           [%e tocaml_app_inits app_inits (tocaml_inits inits (tocaml_step_fun inode)  ) ] ]

