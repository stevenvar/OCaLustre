let make_record_expr loc l =
  let fields =
    List.map (fun ((loc, name, mut, t), default) -> 
                (<:patt< $lid:name$ >>, <:expr< $lid:name$ >>)) l in
  <:expr< { $list:fields$ } >>

let expand_record loc type_name l =
  let type_def = 
    let fields = List.map fst l in
    <:str_item< type $lid:type_name$ = { $list:fields$ } >> in
  let expr_def =
    let record_expr = make_record_expr loc l in
    let f =
      List.fold_right
        (fun ((loc, name, mut, t), default) e ->
           match default with
               None ->
                 <:expr< fun ~ $Ploc.VaVal name$ -> $e$ >>
             | Some x ->
                 <:expr< fun ? ($lid:name$ = $x$) -> $e$ >>)
        l
        <:expr< fun () -> $record_expr$ >> in
    <:str_item< value rec $lid: "create_" ^ type_name$ = $f$ >> in
  <:str_item< declare $type_def$; $expr_def$; end >>

EXTEND
  GLOBAL: Pcaml.str_item;

  Pcaml.str_item: LEVEL "top" [
    [ "record"; type_name = LIDENT; "="; 
      "{"; l = LIST1 field_decl SEP ";"; "}" -> expand_record loc type_name l ]
  ];

  field_decl: [
    [ mut = OPT "mutable";
      name = LIDENT; ":"; t = Pcaml.ctyp; 
      default = OPT [ "="; e = Pcaml.expr LEVEL "simple" -> e ] -> 
        ((loc, name, (mut <> None), t), default) ]
  ];
END;;
