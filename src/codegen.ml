open Parsetree
open Asttypes
open Longident
open Parsing_ast
open Sequential_ast
open Ast_helper
open Sequentialize

let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i^suffix);
    loc = Location.none
  }


let rec tocaml_expression e =
  match e with
  | S_Value (Nil) -> [%expr Obj.magic () ]
  | S_Value (Enum s) ->  Exp.construct {txt = Lident s; loc = Location.none} None
  | S_Value (Integer i) -> Exp.constant (Pconst_integer (string_of_int i,None))
  | S_Value (Float f) -> Exp.constant (Pconst_float (string_of_float f,None))
  | S_Value (Bool true) ->
    Exp.construct {txt= Lident "true" ; loc = Location.none } None
  | S_Value (Bool false) ->
    Exp.construct {txt= Lident "false" ; loc = Location.none } None
  | S_ETuple t -> Exp.tuple (List.map (fun i -> tocaml_expression i) t)
  | S_List l -> [%expr () ]
  | S_Variable i -> [%expr  [%e Exp.ident (lid_of_ident i) ] ]
  | S_Ref i -> [%expr ![%e Exp.ident (lid_of_ident i) ]  ]
  | S_RefDef e -> [%expr ref [%e tocaml_expression e ] ]
  | S_PrefixOp (S_Not, e) -> [%expr not [%e tocaml_expression e ] ]
  | S_PrefixOp (S_Neg, e) -> [%expr ~- [%e tocaml_expression e ] ]
  | S_PrefixOp (S_Negf, e) -> [%expr ~-. [%e tocaml_expression e ] ]
  | S_InfixOp (S_Diff,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <> [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Equals,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] = [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Mod,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] mod [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Plus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] + [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Minus,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] - [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Times,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] * [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Div,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] / [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Plusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] +. [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Minusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] -. [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Timesf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] *. [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Divf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ]  /. [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Inf,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] < [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Infe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] <= [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Sup,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] > [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Supe,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] >= [%e tocaml_expression e2 ]]
  | S_InfixOp (S_Or,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] || [%e tocaml_expression e2 ]]
  | S_InfixOp (S_And,e1,e2) ->
    [%expr [%e tocaml_expression e1 ] && [%e tocaml_expression e2 ]]
  | S_Application (id, num, e) ->
    let e' = tocaml_expression e in
    let n = string_of_int num in
    [%expr [%e (Exp.ident (lid_of_ident (id^n^"_step")))] [%e e' ]]
  | S_Application_init (id,num,e) ->
    let e' = tocaml_expression e in
    [%expr [%e (Exp.ident (lid_of_ident (id)))] [%e e' ]]
  | S_Alternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1) ]]
        [%expr  [%e (tocaml_expression e2) ] ]
        (Some ( [%expr  [%e tocaml_expression e3 ] ] ))
    ]
    ]
  | S_Unit -> [%expr ()]
  | S_Constr _ -> [%expr ()]
  | S_Call e ->
    [%expr [%e e ]]

let rec lident_of_string s =
  {
    txt = Lident s;
    loc = Location.none
  }

let stringloc_of_string s =
  {
    txt = s;
    loc = Location.none;
  }
let tocaml_type s_node = ()

let pat_of_string s =
  { ppat_desc = Ppat_var (stringloc_of_string s);
            ppat_loc = Location.none;
            ppat_attributes = [] }

let rec pat_of_list l =
  match l with
    [] -> { ppat_desc = Ppat_construct (lident_of_string "()", None);
            ppat_loc = Location.none;
            ppat_attributes = [] }
  | h::t ->
    { ppat_desc = Ppat_var (stringloc_of_string h);
      ppat_loc = Location.none;
      ppat_attributes = [] }


let stringloc_of_pattern ?(prefix="") ?(suffix="") p =
  match p.p_desc with
  | Ident i ->
    {
      txt = prefix^i^suffix;
      loc = Location.none;
    }
  | _ -> failwith "no tuple !"


let rec pat_of_pattern p =
  match p.p_desc with
  | Ident i -> { ppat_desc = Ppat_var (stringloc_of_pattern p) ;
                 ppat_loc = p.p_loc ;
                 ppat_attributes = [] }
  | Tuple t ->
    let tl = List.map (fun p -> pat_of_pattern p) t in
    { ppat_desc = Ppat_tuple tl ;
      ppat_loc = p.p_loc ;
      ppat_attributes = [] }
  | PUnit -> { ppat_desc = Ppat_construct (lident_of_string "()" ,None);
               ppat_loc = p.p_loc ;
               ppat_attributes = [] }
  | Typed (p,s) ->
    let core_type = {
       ptyp_desc = Ptyp_constr(lident_of_string s,[]);
     ptyp_loc =  p.p_loc ;
     ptyp_attributes = [];
    }
    in
    {
      ppat_desc = Ppat_constraint (pat_of_pattern p, core_type) ;
      ppat_loc = p.p_loc;
      ppat_attributes = []}  


let tocaml_eq_list el s =
  let tocaml_eq e acc =
    let x = e.s_pattern in
    let sx = pat_of_pattern x in
    let pexpr = tocaml_expression e.s_expression in
    [%expr let [%p sx ] = ([%e pexpr ]) in [%e acc]]
  in
  List.fold_left (fun l e -> tocaml_eq e l) s el


let rec fun_of_list l s =
  match l with 
  | [] -> s
  | h::t ->
    [%expr fun [%p pat_of_string h] -> [%e fun_of_list t s ] ]


let rec tocaml_state_zero s name =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> (name^"_pre_"^s,s)) s.pres in
  let calls = List.map (fun s -> (name^"_"^s^"_state",s^"_state")) s.calls in
  let outs = List.map (fun s -> (name^"_out_"^s,s)) s.outs in
  let l = pres@calls@outs in
  let rec loop l =
    match l with
    |  [] -> []
    | (x,y)::t -> (lident_of_string x, Exp.ident (lid_of_ident y)) :: loop t
  in
  { pexp_desc = Pexp_record (loop l,None);
    pexp_loc = Location.none ; 
    pexp_attributes = []
  }

let tocaml_s_zero (f:s_fun) =
  let name = stringloc_of_string ((string_of_pattern f.s_name)^"_0") in
  let ins = f.s_inputs in
  let eqs = List.rev f.s_eqs in
  let st = tocaml_state_zero f.s_state f.s_name in
  [%stri
    let [%p Pat.var name ] =
      [%e fun_of_list ins (tocaml_eq_list eqs st)]
  ]

let tocaml_node s_node =
  let typ = tocaml_type s_node in
  let f0 = tocaml_s_zero s_node.s_zero in
  f0


