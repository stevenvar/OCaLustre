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

let rec tocaml_imperative_updates e el n =
  let rec loop l =
    match l with
    | [] -> e
    | (e1,e2)::t ->
      let e1 = tocaml_expression e1 n in
      let e2 = tocaml_expression e2 n in
      Exp.sequence [%expr tab.([%e e1]) <- [%e e2]] (loop t)
  in loop el
and tocaml_expression e n =
  let open Parsing_ast in
  match e.i_desc with
  | IValue (String s) -> Ast_convenience.str ~loc:e.i_loc s
  | IValue (Nil) -> { [%expr Obj.magic () ] with pexp_loc=e.i_loc}
  | IValue (Enum s) -> Ast_convenience.constr ~loc:e.i_loc s []
  | IValue (Integer i) -> Ast_convenience.int ~loc:e.i_loc i
  | IValue (Float f) -> Ast_convenience.float ~loc:e.i_loc f
  | IValue (Bool true) -> Ast_convenience.constr ~loc:e.i_loc "true" []
  | IValue (Bool false) ->  Ast_convenience.constr ~loc:e.i_loc "false" []
  | IETuple t -> Ast_convenience.tuple ~loc:e.i_loc
                   (List.map (fun i -> tocaml_expression i n) t)
  | IVariable i -> Ast_convenience.evar ~loc:e.i_loc i
  | IRef i ->  [%expr ![%e Exp.ident ~loc:e.i_loc (lid_of_ident i) ]  ]
  | IRefDef e -> [%expr ref [%e tocaml_expression e n] ]
  | IArray el -> Exp.array ~loc:e.i_loc (List.map (fun e -> tocaml_expression e n) el)
  | IArray_fold (e,f,acc) ->
    [%expr Array.fold_left [%e f] [%e tocaml_expression acc n] ([%e tocaml_expression e n]) ]
  | IArray_map (e,f) ->
    [%expr Array.map [%e f] [%e tocaml_expression e n] ]
  | IArray_get (e,e') ->
    [%expr [%e tocaml_expression e n].([%e tocaml_expression e' n]) ]
  | IImperative_update (e,pe) ->
    let e = tocaml_expression e n in
    [%expr let tab = Array.copy [%e e] in [%e tocaml_imperative_updates e pe n] ; tab ]
  | IPrefixOp (INot, e) -> [%expr not [%e tocaml_expression e n] ]
  | IPrefixOp (INeg, e) -> [%expr ~- [%e tocaml_expression e n] ]
  | IPrefixOp (INegf, e) -> [%expr ~-. [%e tocaml_expression e n] ]
  | IInfixOp (IDiff,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] <> [%e tocaml_expression e2 n ]]
  | IInfixOp (IEquals,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] = [%e tocaml_expression e2 n ]]
  | IInfixOp (IMod,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] mod [%e tocaml_expression e2 n ]]
  | IInfixOp (IPlus,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] + [%e tocaml_expression e2 n ]]
  | IInfixOp (IMinus,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] - [%e tocaml_expression e2 n ]]
  | IInfixOp (ITimes,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] * [%e tocaml_expression e2 n ]]
  | IInfixOp (IDiv,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] / [%e tocaml_expression e2 n ]]
  | IInfixOp (IPlusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] +. [%e tocaml_expression e2 n ]]
  | IInfixOp (IMinusf,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] -. [%e tocaml_expression e2 n ]]
  | IInfixOp (ITimesf,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] *. [%e tocaml_expression e2 n ]]
  | IInfixOp (IDivf,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ]  /. [%e tocaml_expression e2 n ]]
  | IInfixOp (IInf,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] < [%e tocaml_expression e2 n ]]
  | IInfixOp (IInfe,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] <= [%e tocaml_expression e2 n ]]
  | IInfixOp (ISup,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] > [%e tocaml_expression e2 n ]]
  | IInfixOp (ISupe,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] >= [%e tocaml_expression e2 n ]]
  | IInfixOp (IOr,e1,e2) ->
    [%expr [%e tocaml_expression e1 n ] || [%e tocaml_expression e2 n ]]
  | IInfixOp (IAnd,e1,e2) ->
    [%expr [%e tocaml_expression e1 n] && [%e tocaml_expression e2 n ]]
  | IApplication (c,id, num, e) ->
     let rec dispatch n accu =
       if n = 0 then accu
       else dispatch (n-1) ([%expr Obj.magic ()]::accu)
     in

    let e' = tocaml_expression e n in
    let cl = tocaml_cond_list c in
    let n' = string_of_int num in
    let exp = [%expr [%e (Exp.ident (lid_of_ident (id^n'^"_app")))] [%e e' ]] in

    begin
      match c with
      | [] -> exp
      | _ ->
             if n <= 1 then
               [%expr if [%e cl] then [%e exp] else Obj.magic () ]
             else
               let magics =
               { pexp_desc = Parsetree.Pexp_tuple (dispatch n []);
                 pexp_loc = e'.pexp_loc;
                 pexp_attributes = [] }
             in
               [%expr if [%e cl] then [%e exp] else [%e magics ]]
    end
  | ICondact(l,e) ->
    let e' = tocaml_expression e n in
    (* let b' = Ast_convenience.constr (string_of_bool b) [] in *)
    (* let i = Ast_convenience.evar i in *)
    let cl = tocaml_cond_list l in
    [%expr if [%e cl] then [%e e'] else Obj.magic ()]
  | IApplication_init (id,e) ->
    let e' = tocaml_expression e n in
    [%expr [%e (Exp.ident (lid_of_ident (id)))] [%e e']]
  | IAlternative (e1,e2,e3) ->
    [%expr [%e Exp.ifthenelse
        [%expr [%e (tocaml_expression e1 n)]]
        [%expr  [%e (tocaml_expression e2 n)] ]
        (Some ( [%expr  [%e tocaml_expression e3 n]]))
    ]
    ]
  | IUnit -> [%expr ()]
  | IConstr _ -> [%expr ()]
  | ICall (f,el) ->
       let el' = List.map (fun e -> tocaml_expression e n) el in
    (* let pat = { p_desc = PUnit; *)
                (* p_loc = Location.none;} in *)
    (* let n = string_of_int num in *)
    let l = List.map (fun e -> Nolabel,e) el' in
    { pexp_desc = Pexp_apply (Exp.ident (lid_of_ident f),l);
      pexp_loc = Location.none;
      pexp_attributes = [];
    }

(* | _ -> failwith "todo" *)

let rec nb_pattern (p : Parsing_ast.pattern) =
  match p.p_desc with
  | Ident _ | PUnit -> 1
  | Typed (p,_) -> nb_pattern p
  | Tuple pl -> List.fold_left (fun acc x -> (nb_pattern x)+acc) 0 pl

let tocaml_eq_list el acc =
  let tocaml_eq e acc =
    let x = e.i_pattern in
    let ppat = pat_of_pattern x in
    let pexpr = tocaml_expression e.i_expression (nb_pattern x) in
    [%expr let [%p ppat] = [%e pexpr] in [%e acc ]]
  in
  List.fold_left (fun l e -> tocaml_eq e l) acc (List.rev el)

let tocaml_updates l acc =
  let rec aux { i_condition = c; i_pattern = p; i_expression = e } acc =
    let cond = tocaml_cond_list c in
    let p' = expr_of_pattern p in
    let e = tocaml_expression e (nb_pattern p) in
    match c with
    | [] -> [%expr [%e p'] := [%e e]; [%e acc]]
    | _ -> [%expr if [%e cond] then [%e p'] := [%e e]; [%e acc] ]
  in
  List.fold_left (fun acc u -> aux u acc) acc l

let tocaml_step inode =
  let outputs = expr_of_pattern inode.i_outputs in
  let equations = tocaml_eq_list inode.i_step.i_step_equations (tocaml_updates inode.i_step.i_step_updates outputs) in
  [%expr fun [%p pat_of_pattern inode.i_inputs] -> [%e equations]]

let tocaml_inits il acc =
  let aux {i_pattern = p ; i_expression = e}  acc =
      [%expr let [%p pat_of_pattern p] =
               [%e tocaml_expression e (nb_pattern p)] in [%e acc]]
  in
  List.fold_left (fun acc i -> aux i acc) acc il

let tocaml_init i acc =
  tocaml_inits (i.i_init_apps@i.i_init_fby) acc


let create_io_file inode =
  let name = string_of_pattern inode.i_name in
  let file_name = (name^"_io.ml") in
  if not (Sys.file_exists file_name) then
    begin
      let oc = open_out file_name in
      let init = Printf.sprintf "let init_%s () = (* TODO *) \n" name in
      let input_params = string_of_pattern inode.i_inputs in
      let input = Printf.sprintf "let input_%s () = (* TODO *) in %s \n" name input_params in
      let output_params = string_of_pattern inode.i_outputs in
      let output = Printf.sprintf "let output_%s %s = (* TODO *) \n" name output_params in
      output_string oc init;
      if input_params <> "()" then
        output_string oc input;
      output_string oc output;
      raise (Location.Error (Location.error ~loc:inode.i_name.p_loc ("I/O functions for node "^name^" where not available : the file "^file_name^" has been created")))
    end


let tocaml_main inode delay =
  create_io_file inode;
  let name = expr_of_pattern inode.i_name in
  let module_name = (String.capitalize_ascii (string_of_pattern inode.i_name^"_io")) in
  let init_funp = prefix_pattern ~pre:"init_" inode.i_name in
  let init_fun = expr_of_pattern init_funp in
  let input_funp = prefix_pattern ~pre:"input_" inode.i_name in
  let input_fun = expr_of_pattern input_funp in
  let output_funp = prefix_pattern ~pre:"output_" inode.i_name in
  let output_fun = expr_of_pattern output_funp in
  if delay <= 0 then
      let eloop =  if (inode.i_inputs.p_desc <> Parsing_ast.PUnit ) then
                     [%expr
                         [%e init_fun ] ();
                      let main = [%e name] ()  in
                      while true do
                        let [%p pat_of_pattern inode.i_inputs ] = [%e input_fun] () in
                        let [%p pat_of_pattern inode.i_outputs ] =
                          main [%e expr_of_pattern inode.i_inputs] in
                        [%e output_fun] [%e expr_of_pattern inode.i_outputs ]
                      done ]
                   else
                     [%expr
                         [%e init_fun ] ();
                      let main = [%e name] ()  in
                      while true do
                        let [%p pat_of_pattern inode.i_outputs ] =
                          main () in
                        [%e output_fun] [%e expr_of_pattern inode.i_outputs ]
                      done ]
      in
    [%stri
     let () =
       [%e (Exp.open_ Asttypes.Fresh (lid_of_ident module_name) eloop)]
    ]
  else
    let eloop = if (inode.i_inputs.p_desc <> Parsing_ast.PUnit ) then
                  [%expr
                      [%e init_fun ] ();
                   let main = [%e name] ()  in
                   while true do
                     let _delay_ms = millis () in
                     let [%p pat_of_pattern inode.i_inputs ] = [%e input_fun] () in
                     let [%p pat_of_pattern inode.i_outputs ] =
                       main [%e expr_of_pattern inode.i_inputs] in
                     [%e output_fun] [%e expr_of_pattern inode.i_outputs ];
                     let _delay_ms = millis () - _delay_ms in
                     delay([%e (Ast_convenience.int delay) ] - _delay_ms)
                   done
                  ]
                else
                  [%expr
                      [%e init_fun ] ();
                   let main = [%e name] ()  in
                   while true do
                     let _delay_ms = millis () in
                     let [%p pat_of_pattern inode.i_outputs ] =
                       main () in
                     [%e output_fun] [%e expr_of_pattern inode.i_outputs ];
                     let _delay_ms = millis () - _delay_ms in
                     delay([%e (Ast_convenience.int delay) ] - _delay_ms)
                   done
                  ]
    in
    [%stri
     let () =
       [%e (Exp.open_ Asttypes.Fresh (lid_of_ident module_name)
              (eloop))
       ]
    ]

let tocaml_node inode =
  let name = stringloc_of_pattern (inode.i_name) in
  let step = tocaml_step inode in
  [%stri let [%p Pat.var name] =
           fun () ->
             [%e tocaml_init inode.i_init step]
  ]
