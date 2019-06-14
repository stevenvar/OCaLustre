open Parsetree
open Asttypes
open Longident
open Parsing_ast
open Sequential_ast
open Sequential_ast_printer
open Ast_helper
open Sequentialize

let lid_of_ident ?(prefix="") ?(suffix="") i =
  {
    txt = Lident (prefix^i^suffix);
    loc = Location.none
  }

(* creates OCaml's AST Exp from s_expression *)
let rec tocaml_expression e =
  match e.s_e_desc with
  | S_Value (Nil) -> [%expr Obj.magic () ]
  | S_Value (Enum s) ->  Exp.construct {txt = Lident s; loc = Location.none} None
  | S_Value (Integer i) -> Exp.constant (Pconst_integer (string_of_int i,None))
  | S_Value (Float f) -> Exp.constant (Pconst_float (string_of_float f,None))
  | S_Value (String str) -> Exp.constant (Pconst_string (str,None))
  | S_Value (Bool true) ->
    Exp.construct {txt= Lident "true" ; loc = Location.none } None
  | S_Value (Bool false) ->
    Exp.construct {txt= Lident "false" ; loc = Location.none } None
  | S_ETuple t ->
    (* let pat = { p_desc = PUnit; *)
    (*             p_loc = Location.none;} in *)
    (* List.iter (fun e -> print_s_expression Format.std_formatter (e,pat)) t; *)
    Exp.tuple (List.map (fun i -> tocaml_expression i) t)
  | S_Variable i -> [%expr  [%e Exp.ident (lid_of_ident i) ] ]
  | S_Ref i ->
    { pexp_desc = Pexp_field ([%expr state ],
                              lid_of_ident i);
      pexp_loc = Location.none;
      pexp_attributes = [];
    }
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
  | S_List e -> [%expr () ]
  | S_Application (id, num, el) ->
    let el' = List.map tocaml_expression el in
    (* let pat = { p_desc = PUnit; *)
                (* p_loc = Location.none;} in *)
    (* let n = string_of_int num in *)
    let l = List.map (fun e -> Nolabel,e) el' in
    { pexp_desc = Pexp_apply (Exp.ident (lid_of_ident id),l);
      pexp_loc = Location.none;
      pexp_attributes = [];
    }
  | S_Application_init (id,num,el) ->
    let el' = List.map tocaml_expression el in
    (* let pat = { p_desc = PUnit; *)
                (* p_loc = Location.none;} in *)
    (* List.iter (fun e -> print_s_expression Format.std_formatter (e,pat)) el; *)
    (* let n = string_of_int num in *)
    let l = List.map (fun e -> Nolabel,e) el' in
    { pexp_desc = Pexp_apply (Exp.ident (lid_of_ident id),l);
      pexp_loc = Location.none;
      pexp_attributes = [];
    }
  | S_Alternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1) ]]
        [%expr  [%e (tocaml_expression e2) ] ]
        (Some ( [%expr  [%e tocaml_expression e3 ] ] ))
    ]
    ]
  | S_Field (e,s) ->
    [%expr [%e Exp.field
        (tocaml_expression e)
        (lid_of_ident s)
    ]]
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

let rec tocaml_type_record l =
  let rec loop (l,n) =
    match l with
    |  [] -> []
    | h::t ->
      { pld_name = stringloc_of_string h ;
        pld_mutable = Mutable;
        pld_type =
          { ptyp_desc = Ptyp_var (String.make 1 (Char.chr (97+n)));
            ptyp_loc = Location.none;
            ptyp_attributes = [];
          };
        pld_attributes = [];
        pld_loc = Location.none;
      }::(loop (t,n+1))
  in
  loop (l,0)


let tocaml_type_params m =
  let rec loop n =
    if n = m then []
    else
      let ty =
        { ptyp_desc = Ptyp_var (String.make 1 (Char.chr (97+n)));
          ptyp_loc = Location.none;
          ptyp_attributes = [];
        } in
      (ty, Invariant)::loop (n+1)
  in
  loop 0

let tocaml_type (s_t:s_typ) =
  let name = string_of_pattern s_t.s_name in
  let name = name^"_state" in
  let ty =    { ptype_name = stringloc_of_string name;
                ptype_params = tocaml_type_params s_t.s_num;
                ptype_cstrs = [];
                ptype_kind = Ptype_record (tocaml_type_record s_t.s_attr);
                ptype_private = Public;
                ptype_manifest = None;
                ptype_attributes = [];
                ptype_loc = Location.none;
              }
  in
  { pstr_desc = Pstr_type (Recursive, [ty]);
    pstr_loc = Location.none
  }


let rec fun_of_list l s =
  match l with
  | [] -> s
  | h::t ->
    [%expr fun [%p pat_of_string h] -> [%e fun_of_list t s ] ]


(* create state of fun _step *)
let rec tocaml_state_next s name =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> (name^"_pre_"^s,"pre_"^s)) s.pres in
  let outs = List.map (fun s -> (name^"_out_"^s,s)) s.outs in
  let l = pres@outs in
  List.fold_left (fun acc (x,y) ->
      [%expr [%e { pexp_desc = Pexp_setfield ([%expr state],
                                   lident_of_string x,
                                   Exp.ident (lident_of_string y));
        pexp_loc = Location.none;
        pexp_attributes = [];
      } ] ; [%e acc ] ]
    ) [%expr ()] l

(* create state of fun _init *)
let rec tocaml_state_zero s name =
  let name = string_of_pattern name in
  let pres = List.map (fun s -> (name^"_pre_"^s,"pre_"^s)) s.pres in
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

(* create function _init *)
let tocaml_s_zero (f:s_fun) =
  let name = stringloc_of_string ((string_of_pattern f.s_name)^"_init") in
  let ins = f.s_inputs in
  let eqs = List.rev f.s_eqs in
  let st = tocaml_state_zero f.s_state f.s_name in
   match ins with
  | [] -> [%stri
    let [%p Pat.var name] = fun () ->
      [%e tocaml_eq_list eqs st ] ]
  | _ ->
    [%stri
      let [%p Pat.var name ] =
        [%e fun_of_list ins (tocaml_eq_list eqs st)]
    ]

(* create function _step *)
let tocaml_s_next (f:s_fun) =
  let name = stringloc_of_string ((string_of_pattern f.s_name)^"_step") in
  let ins = f.s_inputs in
  let eqs = List.rev f.s_eqs in
  let st = tocaml_state_next f.s_state f.s_name in
  match ins with
  | [] -> [%stri
    let [%p Pat.var name] = fun state -> fun () ->
      [%e tocaml_eq_list eqs st ] ]
  | _ ->
    [%stri
      let [%p Pat.var name ] = fun state ->
        [%e fun_of_list ins (tocaml_eq_list eqs st)]
    ]



let create_io_file inode =
  let name = string_of_pattern inode.s_name in
  let file_name = (name^"_io.ml") in
  if not (Sys.file_exists file_name) then
    begin
      let oc = open_out file_name in
      let init = Printf.sprintf "(* initialization function *)\nlet initialize () = (* TODO *) () \n" in
      let input_params = inode.s_zero.s_inputs in
      let inputs = List.map (fun x -> Printf.sprintf "let input_%s_%s () = (* TODO *) in %s \n" name x x) input_params in
      let output_params = List.fold_left (fun acc x -> x^" "^acc) "" inode.s_next.s_outputs in
      let output = Printf.sprintf "let output_%s %s = (* TODO *) \n" name output_params in
      output_string oc init;
      output_string oc "(* Input functions *)\n";
      List.iter (fun fi -> output_string oc fi) inputs;
      output_string oc "(* Output function *)\n";
      output_string oc output;
      raise (Location.Error (Location.error ~loc:inode.s_name.p_loc ("I/O functions for node "^name^" where not available : the file "^file_name^" has been created")))
    end


open Tools

let rec all_input_funs name l e =
  match l with
  | [] -> e
  | h::t ->
     let p = { p_desc = Ident h ; p_loc = Location.none } in
     let fp = prefix_pattern ~pre:("input_"^name^"_") p in
     let f = expr_of_pattern fp in
     [%expr let [%p pat_of_pattern p ] = [%e f ] () in
                [%e all_input_funs name t e ] ]




let tocaml_main inode delay =
  create_io_file inode;
  let name = string_of_pattern inode.s_name in
  let module_name = (String.capitalize_ascii (string_of_pattern inode.s_name^"_io")) in
  (* let initialize_funp = suffix_pattern ~suf:"_initialize" inode.s_name in *)
  (* let initialize_fun = expr_of_pattern initialize_funp in *)
  let init_funp = suffix_pattern ~suf:"_init" inode.s_name in
  let init_fun = expr_of_pattern init_funp in
  let inputsp = List.map (fun x -> { p_desc = Ident x ; p_loc = Location.none }) inode.s_zero.s_inputs in
  let inputs = List.map expr_of_pattern inputsp in
  let inputs = List.map (fun x -> (Nolabel,x)) inputs in
  let output_funp = prefix_pattern ~pre:"output_" inode.s_name in
  let output_fun = expr_of_pattern output_funp in
  let apply_init = Exp.apply init_fun inputs in
  let update_funp = suffix_pattern ~suf:"_step" inode.s_name in
  let update_fun = expr_of_pattern update_funp in
  let state = {p_desc = Ident "_st"; p_loc = Location.none} in
  let update_inputsp = state::inputsp in
  let update_inputs = List.map expr_of_pattern update_inputsp in
  let update_inputs = List.map (fun x -> (Nolabel,x)) update_inputs in
  let apply_update = Exp.apply update_fun update_inputs in
  let state_expr = expr_of_pattern state  in
  let outputs = List.map (fun x -> (Exp.field state_expr (lid_of_ident (name^"_out_"^x)) )) inode.s_next.s_outputs in
  let outputs = List.map (fun x -> (Nolabel,x)) outputs in
  let apply_output = Exp.apply output_fun outputs in

  let e = [%expr let _st = [%e apply_init ] in
                     [%e apply_output] ;
                     while true do
                       [%e all_input_funs name inode.s_zero.s_inputs
                       apply_update ] ;
                       [%e apply_output ] done ] in
  let eloop =  [%expr
                   initialize ();

                [%e all_input_funs name inode.s_zero.s_inputs e] ]

  in
  [%stri
   let () =
     [%e (Exp.open_ Asttypes.Fresh (lid_of_ident module_name) eloop)]
  ]



let tocaml_node s_node is_main d =
  let typ = tocaml_type s_node.s_type in
  let f0 = tocaml_s_zero s_node.s_zero in
  let fn = tocaml_s_next s_node.s_next in
  if is_main then
    let fmain = tocaml_main s_node d in
    [typ;f0;fn;fmain]
  else
    [typ;f0;fn]
