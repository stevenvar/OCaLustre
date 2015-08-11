open Parsetree
open Asttypes
open Longident
open Ast

type constant = Parsetree.expression 
  
type imp_init = (ident * imp_expr option) list 
  
and
  imp_expr =
  | IValue of constant
  | IVariable of ident
  | IRef of ident
  | IInfixOp of imp_infop * imp_expr * imp_expr
  | IPrefixOp of imp_preop * imp_expr
  | IAlternative of imp_expr * imp_expr * imp_expr
  | IApplication of ident * imp_expr list
and
  imp_infop =
  | IPlus
  | IMinus
  | ITimes
  | IDiv
and
  imp_preop =
  | IPre
  | INot
    
type imp_equation =  {
  i_pattern : ident;
  i_expression : imp_expr;
} 

type imp_step = {
  i_equations : imp_equation list;
  i_updates : (ident * imp_expr option) list;
}

type imp_node = {
  i_name : ident;
  i_inputs : ident list;
  i_outputs : ident list;
  i_inits : imp_init;
  i_step_fun : imp_step;
}

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
 
let rec compile_expression exp =
  let compile_preop op =
    match op with
    | Pre -> IPre
    | Not -> INot
  in
  let compile_infop op =
    match op with
    | Plus -> IPlus
    | Minus -> IMinus
    | Times -> ITimes
    | Div -> IDiv
    | _-> failwith "wrong operator" 
  in
  match exp with
  | Value c -> IValue c 
  | Variable v -> IVariable v
  | Ref v -> IRef v
  | InfixOp (op,e1,e2) -> IInfixOp(compile_infop op,
                                compile_expression e1,
                                   compile_expression e2)
  | PrefixOp (op, e) -> IPrefixOp (compile_preop op, compile_expression e)
  | Alternative (e1,e2,e3) ->  IAlternative (compile_expression e1,
                                             compile_expression e2,
                                             compile_expression e3)
  | Application (id, el) -> IApplication (id,
                                          List.map (compile_expression) el)

let printml_string fmt p =
  Format.fprintf fmt "%s" p



let rec printml_expression fmt exp =
  let printml_preop fmt op =
    match op with
    | IPre -> Format.fprintf fmt "pre "
    | INot -> Format.fprintf fmt "not "
  in
  let printml_infop fmt op =
    match op with
    | IPlus -> Format.fprintf fmt " + "
    | ITimes -> Format.fprintf fmt " * "
    | IDiv -> Format.fprintf fmt " / "
    | IMinus -> Format.fprintf fmt " - "
  in
  let rec printml_expressions fmt el =
    match el with
    | [] -> ()
    | e::[] -> Format.fprintf fmt "%a"
                 printml_expression e
    | e::tl -> Format.fprintf fmt "%a,%a"
                 printml_expression e
                 printml_expressions tl
  in 
  match exp with
  | IValue c -> Pprintast.expression fmt c 
  | IVariable v ->  Format.fprintf fmt "%s" v.content
  | IRef v -> Format.fprintf fmt "Option.get (!%s)" v.content
  | IInfixOp (op,e1,e2) -> Format.fprintf fmt "%a %a %a"
                             printml_expression e1
                             printml_infop op
                             printml_expression e2
  | IPrefixOp (op,e) -> Format.fprintf fmt "%a%a"
                          printml_preop op
                          printml_expression e
  | IAlternative (e1,e2,e3) -> Format.fprintf fmt "if %a then %a else %a"
                                 printml_expression e1
                                 printml_expression e2
                                 printml_expression e3
  | IApplication (s, el) -> Format.fprintf fmt "%a(%a)"
                              printml_string s.content
                              printml_expressions el

let printml_inits fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | None -> Format.fprintf fmt "let %a = ref None in\n"
                  printml_string s.content
      | Some x -> Format.fprintf fmt "let %a = ref (Some %a) in\n"
                    printml_string s.content
                    printml_expression x
    end 
  in
  List.iter (fun i -> printml_init fmt i) il

let printml_updates fmt il =
  let printml_init fmt (s,e) =
    begin match e with
      | None -> Format.fprintf fmt "%a := None;\n"
                  printml_string s.content
      | Some x -> Format.fprintf fmt "%a := (Some %a);\n"
                    printml_string s.content
                    printml_expression x
    end 
  in
  List.iter (fun i -> printml_init fmt i) il


let printml_equations fmt el = 
  let printml_equation fmt e =
    Format.fprintf fmt "let %a = %a in \n"
      printml_string e.i_pattern.content
      printml_expression e.i_expression
  in
  List.iter (fun x -> printml_equation fmt x) el
  
let rec printml_io fmt il =
  match il with
  |  [] -> ()
  | s::[] -> Format.fprintf fmt "%s" s.content
  | s::tl -> Format.fprintf fmt "%a,%a"
               printml_string s.content
               printml_io tl
               
let printml_step fmt node =
  let name = node.i_name in
  let inputs = node.i_inputs in
  let outputs = node.i_outputs in 
  let step = node.i_step_fun in 
  Format.fprintf fmt "let %a_step (%a) = \n %a %a (%a)"
    printml_string name.content
    printml_io inputs
    printml_equations step.i_equations
    printml_updates step.i_updates
    printml_io outputs
    
let printml_node fmt node =
  Format.fprintf fmt "\n let %a =\n%a %a \nin %a_step \n\n"
    printml_string node.i_name.content
    printml_inits node.i_inits
    printml_step node
    printml_string node.i_name.content


let generate_inits node =
  let generate_from_e equation l =
    match equation.expression with
    | PrefixOp (Pre, x) -> (equation.pattern,None)::l
    | InfixOp (Arrow, x, y) -> (equation.pattern,
                                Some (compile_expression x))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [] equations
      in
  (* list of couples (string, value) *) 
      generate_from_el node.equations


let generate_updates node =
  let generate_from_e equation l =
    match equation.expression with
    | PrefixOp (Pre, x) ->
      (equation.pattern, Some (compile_expression x) )::l
    | InfixOp (Arrow, x, y) -> (equation.pattern, Some (compile_expression y))::l
    | _ -> l
  in 
  let generate_from_el equations = 
    List.fold_left (fun l e -> let l = generate_from_e e l in l ) [ ] equations
  in
  (* list of couples (string, value) *) 
  generate_from_el node.equations       

let compile_io l =
  List.map (fun io -> io) l
    
let compile_equation e = {
  i_pattern = e.pattern;
  i_expression = compile_expression e.expression;
}

let remove_forbidden_op_list el =
  let remove_forbidden_op e l =
    match e.expression with
    | PrefixOp (Pre, x) -> l
    | InfixOp (Arrow,x,y) -> l
    | _ -> e::l
  in List.fold_left (fun l e -> let l = remove_forbidden_op e l in l ) [] el  

let compile_node node =
  {
    i_name = node.name;
    i_inputs = compile_io node.inputs;
    i_outputs =compile_io node.outputs;
    i_inits = (generate_inits node);
    i_step_fun =
      {
        i_equations =
          List.map (compile_equation) (remove_forbidden_op_list node.equations);
        i_updates = ( generate_updates node);
      };
  }


let rec tocaml_expression e =
  Ast_helper.( 
    match e with
    | IValue v -> v
    | IVariable i -> [%expr [%e Exp.ident (ident_to_lid i) ]  ]
    | IRef i -> [%expr Option.get ![%e Exp.ident (ident_to_lid i) ]  ]
    | IInfixOp (IPlus,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] + [%e tocaml_expression e2 ]]
    | IInfixOp (IMinus,e1,e2) ->
       [%expr [%e tocaml_expression e1 ] - [%e tocaml_expression e2 ]]
    | IInfixOp (ITimes,e1,e2) ->
       [%expr [%e tocaml_expression e1 ] * [%e tocaml_expression e2 ]]
    | IInfixOp (IDiv,e1,e2) ->
      [%expr [%e tocaml_expression e1 ] / [%e tocaml_expression e2 ]]
    | IPrefixOp (op, e) -> [%expr let () = () in () ]
    | IAlternative (e1,e2,e3) -> 
      [%expr [%e Exp.ifthenelse
                (tocaml_expression e1) 
                (tocaml_expression e2) 
                (Some (tocaml_expression e3))  ] ]
    | IApplication (id, el) ->
      Exp.apply
        (Exp.ident (ident_to_lid id))
        [("",Exp.tuple (List.map (fun x -> tocaml_expression x) el))]
    | _ -> assert false )

let tocaml_inits il acc =
  let tocaml_init (s,e) acc =
    match e with
    | None ->
      [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc s)] =
               ref None in [%e acc ] ]  
    | Some x ->
      [%expr let [%p Ast_helper.Pat.var (ident_to_stringloc s)] =
               ref (Some [%e tocaml_expression x ]) in [%e acc ] ]
  in List.fold_left (fun l i -> tocaml_init i l ) acc il 

let tocaml_inputs il =
  List.map (fun i -> Ast_helper.Pat.var (ident_to_stringloc i) ) il
    
let tocaml_outputs ol =
  List.map (fun o -> Ast_helper.Exp.ident (ident_to_lid o) ) ol 

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    let ppat = ident_to_stringloc e.i_pattern in
    let pexpr = tocaml_expression e.i_expression in 
    [%expr let [%p Ast_helper.Pat.var ppat ] = ( [%e pexpr ] ) in  [%e acc ] ]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc el

let tocaml_updates ul acc =
  let tocaml_update (s,e) acc =
    match e with
    | None ->
      [%expr [%p Ast_helper.Exp.ident (ident_to_lid s)] :=
               None ; [%e acc ] ]  
    | Some x ->
      [%expr [%e Ast_helper.Exp.ident (ident_to_lid s)] :=
               (Some [%e tocaml_expression x ]) ; [%e acc ] ]
  in
  List.fold_left (fun l u -> tocaml_update u l) acc ul
    

let tocaml_step_fun node =
  let pname = ident_to_stringloc_suffix node.i_name "_step" in
  let out =
    match node.i_outputs with
    | [x] -> [%expr [%e Ast_helper.Exp.ident (ident_to_lid x)]]
    | _ -> [%expr [%e
                     Ast_helper.Exp.tuple (tocaml_outputs node.i_outputs)
                  ]]
  in
  let ups = [%expr [%e tocaml_updates node.i_step_fun.i_updates out ]] in
  match node.i_inputs with
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
  let inits = inode.i_inits in
  
  [%stri let [%p Ast_helper.Pat.var pname ] =
           [%e (tocaml_inits inits (tocaml_step_fun inode)  ) ] ]

