open Types
    
let string_of_path p =
  let open Path in 
  match p with
  | Pident i -> i.name
  | Pdot _ -> "."
  | Papply _ -> "*"
    
let rec print_type_expr fmt e=
  let p = print_type_expr in
  let ff = Format.fprintf in
   let rec print_list fmt l =
    match l with
    | [] -> ()
    | [x] -> ff fmt "%a" p x
    | x::xs -> ff fmt "%a,%a" p x print_list xs
  in
  match e.desc with
  | Tvar None -> ff fmt "?"
  | Tvar (Some x) -> ff fmt "%s" x
  | Tarrow (_,e1,e2,_) -> ff fmt "%a -> %a" p e1 p e2
  | Ttuple l -> ff fmt "(%a)" print_list l
  | Tconstr (p,l,_) -> ff fmt "%s" (string_of_path p)
  | Tobject _ -> ff fmt "object"
  | Tfield _ -> ff fmt "field" 
  | Tnil ->ff fmt "nil"
  | Tlink e -> p fmt e
  | Tsubst _-> ff fmt "susbst"
  | Tvariant _-> ff fmt "variant"
  | Tunivar _->ff fmt "univar"
  | Tpoly _ -> ff fmt "poly"
  | Tpackage _ -> ff fmt "package"
  
let env_growing = ref [] 
  let print_type st =
    let env = Compmisc.initial_env () in
    let env = List.fold_left (fun acc x -> Env.add_signature x acc) env !env_growing in
    let (tf,ts,_) =Typemod.type_structure env  st Location.none in
    env_growing := ts::!env_growing ;
        let str = List.hd ts in
        (match str with
         | Sig_value (_,v) ->
           print_type_expr Format.std_formatter v.val_type;
        | _ -> print_string "other")
