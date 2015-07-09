(* Dump a human readable abstract syntax tree of OCaml. This is 
   especially useful if you want to understand what ast_pattern to match
   in the AST for building a custom OCaml AST analyzer ( cf Pr_analyze ) 
*)

open MLast;;
open Format;;

let ast_loc ppf loc = 
  fprintf ppf "N/A"
;;

let ast_list f ppf lst =
  fprintf ppf "[@[";
  (
    match lst with
      [] ->
        ()
    | hd :: tl ->
        fprintf ppf "%a" f hd;
        List.iter (
          fun x ->
            fprintf ppf ";@ %a" f x
        ) tl;
  );
  fprintf ppf "@]]"
;;

let ast_opt f ppf o = 
  match o with 
    Some x ->
      fprintf ppf "Some(@[%a@])"
      f x
  | None ->
      fprintf ppf "None"
;;

let ast_string ppf s =
  fprintf ppf "%S" s
;;

let rec ast_ctyp ppf c =
  match c with
    TyAcc (loc,c1,c2) -> 
      fprintf ppf "TyAcc(@[%a,@ %a,@ ,%a@])" 
      ast_loc loc 
      ast_ctyp c1 
      ast_ctyp c2
  | TyAli (loc,c1,c2) -> 
      fprintf ppf "TyAli(@[%a,@ %a,@ ,%a@])" 
      ast_loc loc 
      ast_ctyp c1 
      ast_ctyp c2
  | TyAny loc -> 
      fprintf ppf "TyAny(@[%a@])"
      ast_loc loc
  | TyApp (loc,c1,c2) -> 
      fprintf ppf "TyApp(@[%a,@ %a,@ %a@])" 
      ast_loc loc
      ast_ctyp c1
      ast_ctyp c2
  | TyArr (loc,c1,c2) -> 
      fprintf ppf "TyArr(@[%a,@ %a,@ %a@])" 
      ast_loc loc
      ast_ctyp c1
      ast_ctyp c2
  | TyCls (loc,lst) -> 
      fprintf ppf "TyCls(@[%a,@ %a@])" 
      ast_loc loc
      (ast_list ast_string) lst
  | TyLab (loc,str,c1) -> 
      fprintf ppf "TyLab(@[%a,@ %S,@ %a@])" 
      ast_loc loc
      str
      ast_ctyp c1
  | TyLid (loc,str) -> 
      fprintf ppf "TyLid(@[%a,@ %S@])" 
      ast_loc loc
      str
  | TyMan (loc,c1,c2) -> 
      fprintf ppf "TyMan(@[%a,@ %a,@ %a@])" 
      ast_loc loc
      ast_ctyp c1
      ast_ctyp c2
  | TyObj (loc,lst,b) -> 
      fprintf ppf "TyObj(@[%a,@ %a,@ %B@])" 
      ast_loc loc
      ( 
        ast_list (
          fun ppf (str,c1) ->
            fprintf ppf "(@[%S,@ %a@])"
            str
            ast_ctyp c1 
          )
      ) lst
      b
  | TyOlb (loc,str,c1) -> 
      fprintf ppf "TyOlb(@[%a,@ %S,@ %a@])" 
      ast_loc loc
      str
      ast_ctyp c1
  | TyPol (loc,lst,c1) -> 
      fprintf ppf "TyPol(@[%a,@ %a,@ %a@])" 
      ast_loc loc
      (ast_list ast_string) lst
      ast_ctyp c1
  | TyQuo (loc,str) -> 
      fprintf ppf "TyQuo(@[%a,@ %S@])" 
      ast_loc loc
      str
  | TyRec (loc,b,lst) -> 
      fprintf ppf "TyRec(@[%a,@ %B,@ %a@])" 
      ast_loc loc
      b
      (
        ast_list ( 
          fun ppf (loc,str,b,c1) -> 
            fprintf ppf "(@[%a,@ %S,@ %B,@ %a@])"
            ast_loc loc
            str
            b
            ast_ctyp c1 
          )
      ) lst
  | TySum (loc,b,lst) -> 
      fprintf ppf "TySum(@[%a,@ %B,@ %a@])" 
      ast_loc loc
      b
      (
        ast_list ( 
          fun ppf (loc,str,lst) -> 
            fprintf ppf "(@[%a,@ %S,@ %a@])"
            ast_loc loc
            str
            (ast_list ast_ctyp) lst
          )
      ) lst
  | TyTup (loc,lst) -> 
      fprintf ppf "TyTup(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_ctyp) lst
  | TyUid (loc,str)-> 
      fprintf ppf "TyUid(@[%a,@ %S@])" 
      ast_loc loc
      str
  | TyVrn (loc,lst,opt) -> 
      fprintf ppf "TyVrn(@[%a,@ %a,@ %a@])" 
      ast_loc loc
      (ast_list ast_row_field) lst
      (ast_opt (ast_opt (ast_list ast_string))) opt
and ast_row_field ppf r =
  match r with
    RfTag (str,b,lst) -> 
      fprintf ppf "RfTag(@[%S,@ %B,@ %a@])"
      str
      b
      (ast_list ast_ctyp) lst
  | RfInh c1 -> 
      fprintf ppf "RfInh(@[%a@])"
      ast_ctyp c1
;;

let ast_class_infos ast_type ppf ci =
  fprintf ppf "{@[ciLoc = %a;@ ciVir = %B;@ ciPrm = (@[%a, %a@]);@ ciNam = %S;@ ciExp = %a@]}"
  ast_loc ci.ciLoc
  ci.ciVir
  ast_loc (fst ci.ciPrm)
  (
    ast_list
    ( fun ppf (str,(b1,b2)) ->
      fprintf ppf "(@[%S,@ (@[%B,@ %B@])@])"
      str
      b1
      b2
    )
  ) (snd ci.ciPrm)
  ci.ciNam
  ast_type ci.ciExp
;;

let rec ast_patt ppf p =
  match p with
    PaAcc (loc,p1,p2) ->
      fprintf ppf "PaAcc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_patt p2
  | PaAli (loc,p1,p2) ->
      fprintf ppf "PaAli(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_patt p2
  | PaAnt (loc,p1) ->
      fprintf ppf "PaAnt(@[%a,@ %a@])"
      ast_loc loc
      ast_patt p1
  | PaAny loc ->
      fprintf ppf "PaAny(@[%a@])"
      ast_loc loc
  | PaApp (loc,p1,p2) ->
      fprintf ppf "PaApp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_patt p2
  | PaArr (loc,lst) -> 
      fprintf ppf "PaArr(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_patt) lst
  | PaChr (loc,str) -> 
      fprintf ppf "PaChr(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaInt (loc,str) -> 
      fprintf ppf "PaInt(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaInt32 (loc,str) -> 
      fprintf ppf "PaInt32(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaInt64 (loc,str) -> 
      fprintf ppf "PaInt64(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaNativeInt (loc,str) -> 
      fprintf ppf "PaNativeInt(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaFlo (loc,str) -> 
      fprintf ppf "PaFlo(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaLab (loc,str,opt) -> 
      fprintf ppf "PaLab(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_opt ast_patt) opt
  | PaLid (loc,str) -> 
      fprintf ppf "PaLid(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaOlb (loc,str,opt) -> 
      fprintf ppf "PaOlb(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ( 
        ast_opt (
          fun ppf (p1,opt) -> 
            fprintf ppf "%a,@ %a" 
            ast_patt p1 
            (ast_opt ast_expr) opt
          )
      ) opt
  | PaOrp (loc,p1,p2) -> 
      fprintf ppf "PaOrp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_patt p2
  | PaRng (loc,p1,p2) -> 
      fprintf ppf "PaRng(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_patt p2
  | PaRec (loc,lst) -> 
      fprintf ppf "PaRec(@[%a,@ %a@])"
      ast_loc loc
      (
        ast_list (
          fun ppf (p1,p2) -> 
            fprintf ppf "(@[%a,@ %a@])"
            ast_patt p1
            ast_patt p2
          ) 
      ) lst
  | PaStr (loc,str) -> 
      fprintf ppf "PaStr(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaTup (loc,lst) -> 
      fprintf ppf "PaTup(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_patt) lst
  | PaTyc (loc,p1,c1) -> 
      fprintf ppf "PaTyc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_ctyp c1
  | PaTyp (loc,lst) -> 
      fprintf ppf "PaTyp(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst
  | PaUid (loc,str) -> 
      fprintf ppf "PaUid(@[%a,@ %S@])"
      ast_loc loc
      str
  | PaVrn (loc,str) -> 
      fprintf ppf "PaVrn(@[%a,@ %S@])"
      ast_loc loc
      str
and ast_expr ppf e =
  match e with
    ExAcc (loc,e1,e2) -> 
      fprintf ppf "ExAcc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
  | ExAnt (loc,e1) -> 
      fprintf ppf "ExAnt(@[%a,@ %a@])"
      ast_loc loc
      ast_expr e1
  | ExApp (loc,e1,e2) -> 
      fprintf ppf "ExApp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
  | ExAre (loc,e1,e2) -> 
      fprintf ppf "ExAre(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
  | ExArr (loc,lst) -> 
      fprintf ppf "ExArr(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_expr) lst
  | ExAsf loc -> 
      fprintf ppf "ExAsf(@[%a@])"
      ast_loc loc
  | ExAsr (loc,e1) -> 
      fprintf ppf "ExAsr(@[%a,@ %a@])"
      ast_loc loc
      ast_expr e1
  | ExAss (loc,e1,e2) -> 
      fprintf ppf "ExAss(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
  | ExChr (loc,str) -> 
      fprintf ppf "ExChr(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExCoe (loc,e1,opt,c1) -> 
      fprintf ppf "ExCoe(@[%a,@ %a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      (ast_opt ast_ctyp) opt
      ast_ctyp c1
  | ExFlo (loc,str) -> 
      fprintf ppf "ExFlo(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExFor (loc,str,e1,e2,b,lst) ->
      fprintf ppf "ExFor(@[%a,@ %S,@ %a,@ %a,@ %B,@ %a@])"
      ast_loc loc
      str
      ast_expr e1
      ast_expr e2
      b
      (ast_list ast_expr) lst
  | ExFun (loc,lst) ->
      fprintf ppf "ExFun(@[%a,@ %a@])"
      ast_loc loc
      (
        ast_list ( 
          fun ppf (p1,opt,e1) ->
            fprintf ppf "(@[%a,@ %a,@ %a@])"
            ast_patt p1
            (ast_opt ast_expr) opt
            ast_expr e1
          ) 
      ) lst
  | ExIfe (loc,e1,e2,e3) ->
      fprintf ppf "ExIfe(@[%a,@ %a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
      ast_expr e3
  | ExInt (loc,str) -> 
      fprintf ppf "ExInt(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExInt32 (loc,str) -> 
      fprintf ppf "ExInt32(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExInt64 (loc,str) -> 
      fprintf ppf "ExInt64(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExNativeInt (loc,str) -> 
      fprintf ppf "ExNativeInt(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExLab (loc,str,opt) -> 
      fprintf ppf "ExLab(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_opt ast_expr) opt
  | ExLaz (loc,e1) -> 
      fprintf ppf "ExLaz(@[%a,@ %a@])"
      ast_loc loc
      ast_expr e1
  | ExLet (loc,b,lst,e1) ->
      fprintf ppf "ExLet(@[%a,@ %B,@ %a,@ %a@])"
      ast_loc loc
      b
      (
        ast_list 
        ( fun ppf (p1,e1) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_patt p1
          ast_expr e1
        ) 
      ) lst
      ast_expr e1
  | ExLid (loc,str) -> 
      fprintf ppf "ExLid(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExLmd (loc,str,me1,e1) ->
      fprintf ppf "ExLmd(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      ast_module_expr me1
      ast_expr e1
  | ExMat (loc,e1,lst) ->
      fprintf ppf "ExMat(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      (
        ast_list 
        ( fun ppf (p1,opt,e1) ->
          fprintf ppf "(@[%a,@ %a,@ %a@])"
          ast_patt p1
          (ast_opt ast_expr) opt
          ast_expr e1
        ) 
      ) lst
  | ExNew (loc,lst) -> 
      fprintf ppf "ExNew(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst
  | ExObj (loc,opt,lst) ->
      fprintf ppf "ExObj(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_opt ast_patt) opt
      (ast_list ast_class_str_item) lst 
  | ExOlb (loc,str,opt) -> 
      fprintf ppf "ExOlb(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_opt ast_expr) opt
  | ExOvr (loc,lst) -> 
      fprintf ppf "ExOvr(@[%a,@ %a@])"
      ast_loc loc
      ( 
        ast_list
        ( fun ppf (str,e1) ->
          fprintf ppf "(@[%S,@ %a@])"
          str
          ast_expr e1
        ) 
      ) lst
  | ExRec (loc,lst,opt) ->
      fprintf ppf "ExRec(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (
        ast_list
        ( fun ppf (p1,e1) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_patt p1
          ast_expr e1
        ) 
      ) lst
      (ast_opt ast_expr) opt
  | ExSeq (loc,lst) ->
      fprintf ppf "ExSeq(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_expr) lst
  | ExSnd (loc,e1,str) -> 
      fprintf ppf "ExSnd(@[%a,@ %a,@ %S@])"
      ast_loc loc
      ast_expr e1
      str
  | ExSte (loc,e1,e2) ->
      fprintf ppf "ExSte(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_expr e2
  | ExStr (loc,str) -> 
      fprintf ppf "ExStr(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExTry (loc,e1,lst) ->
      fprintf ppf "ExTry(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      (
        ast_list 
        ( fun ppf (p1,opt,e1) ->
          fprintf ppf "(@[%a,@ %a,@ %a@])"
          ast_patt p1
          (ast_opt ast_expr) opt
          ast_expr e1
        ) 
      ) lst
  | ExTup (loc,lst) ->
      fprintf ppf "ExTup(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_expr) lst
  | ExTyc (loc,e1,c1) ->
      fprintf ppf "ExTyc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      ast_ctyp c1
  | ExUid (loc,str) -> 
      fprintf ppf "ExUid(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExVrn (loc,str) -> 
      fprintf ppf "ExVrn(@[%a,@ %S@])"
      ast_loc loc
      str
  | ExWhi (loc,e1,lst) ->
      fprintf ppf "ExWhi(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_expr e1
      (ast_list ast_expr) lst
and ast_module_type ppf mt =
  match mt with
    MtAcc (loc,mt1,mt2) ->
      fprintf ppf "MtAcc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_type mt1
      ast_module_type mt2
  | MtApp (loc,mt1,mt2) ->
      fprintf ppf "MtApp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_type mt1
      ast_module_type mt2
  | MtFun (loc,str,mt1,mt2) ->
      fprintf ppf "MtFun(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      ast_module_type mt1
      ast_module_type mt2
  | MtLid (loc,str) -> 
      fprintf ppf "MtLid(@[%a,@ %S@])"
      ast_loc loc
      str
  | MtQuo (loc,str) -> 
      fprintf ppf "MtQuo(@[%a,@ %S@])"
      ast_loc loc
      str
  | MtSig (loc,lst) -> 
      fprintf ppf "MtSig(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_sig_item) lst
  | MtUid (loc,str) -> 
      fprintf ppf "MtUid(@[%a,@ %S@])"
      ast_loc loc
      str
  | MtWit (loc,mt1,lst) ->
      fprintf ppf "MtWit(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_type mt1
      (ast_list ast_with_constr) lst
and ast_sig_item ppf sg =
  match sg with
    SgCls (loc,lst) -> 
      fprintf ppf "SgCls(@[%a,@ %a@])"
      ast_loc loc
      (ast_list (ast_class_infos ast_class_type)) lst
  | SgClt (loc,lst) -> 
      fprintf ppf "SgClt(@[%a,@ %a@])"
      ast_loc loc
      (ast_list (ast_class_infos ast_class_type)) lst
  | SgDcl (loc,lst) ->
      fprintf ppf "SgDcl(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_sig_item) lst
  | SgDir (loc,str,opt) ->
      fprintf ppf "SgDir(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_opt ast_expr) opt
  | SgExc (loc,str,lst) ->
      fprintf ppf "SgExc(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_list ast_ctyp) lst
  | SgExt (loc,str,c1,lst) ->
      fprintf ppf "SgExt(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      ast_ctyp c1
      (ast_list ast_string) lst
  | SgInc (loc,mt1) ->
      fprintf ppf "SgInc(@[%a,@ %a@])"
      ast_loc loc
      ast_module_type mt1
  | SgMod (loc,str,mt1) ->
      fprintf ppf "SgMod(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ast_module_type mt1
  | SgRecMod (loc,lst) ->
      fprintf ppf "SgRecMod(@[%a,@ %a@])"
      ast_loc loc
      (
        ast_list 
        ( fun ppf (str,mt1) ->
          fprintf ppf "(@[%S,@ %a@])"
          str
          ast_module_type mt1
        ) 
      ) lst
  | SgMty (loc,str,mt1) ->
      fprintf ppf "SgMty(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ast_module_type mt1
  | SgOpn (loc,lst) ->
      fprintf ppf "SgOpn(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst
  | SgTyp (loc,lst) ->
      fprintf ppf "SgTyp(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_type_decl) lst
  | SgUse (loc,str,lst) ->
      fprintf ppf "SgUse(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (
        ast_list 
        ( fun ppf (sg1,loc) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_sig_item sg1
          ast_loc loc
        ) 
      ) lst
  | SgVal (loc,str,c1) ->
      fprintf ppf "SgVal(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ast_ctyp c1
and ast_with_constr ppf wc =
  match wc with
    WcTyp (loc,lst1,lst2,c1) ->
      fprintf ppf "WcTyp(@[%a,@ %a,@ %a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst1
      (
        ast_list
        ( fun ppf (str,(b1,b2)) ->
          fprintf ppf "(@[%S,@ (@[%B,@ %B@])@])"
          str
          b1
          b2
        )
      ) lst2
      ast_ctyp c1      
  | WcMod (loc,lst,me1) ->
      fprintf ppf "WcMod(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst
      ast_module_expr me1
and ast_module_expr ppf me =
  match me with
    MeAcc (loc,me1,me2) ->
      fprintf ppf "MeAcc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_expr me1
      ast_module_expr me2
  | MeApp (loc,me1,me2) ->
      fprintf ppf "MeApp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_expr me1
      ast_module_expr me2
  | MeFun (loc,str,mt1,me1) ->
      fprintf ppf "MeFun(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      ast_module_type mt1
      ast_module_expr me1
  | MeStr (loc,lst) ->
      fprintf ppf "MeStr(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_str_item) lst
  | MeTyc (loc,me1,mt1) ->
      fprintf ppf "MeTyc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_module_expr me1
      ast_module_type mt1
  | MeUid (loc,str) ->
      fprintf ppf "MeUid(@[%a,@ %S@])"
      ast_loc loc
      str
and ast_str_item ppf st =
  match st with
    StCls (loc,lst) ->
      fprintf ppf "StCls(@[%a,@ %a@])"
      ast_loc loc
      (ast_list (ast_class_infos ast_class_expr)) lst
  | StClt (loc,lst) ->
      fprintf ppf "StClt(@[%a,@ %a@])"
      ast_loc loc
      (ast_list (ast_class_infos ast_class_type)) lst
  | StDcl (loc,lst) ->
      fprintf ppf "StDcl(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_str_item) lst
  | StDir (loc,str,opt) ->
      fprintf ppf "StDir(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (ast_opt ast_expr) opt
  | StExc (loc,str,lst1,lst2) ->
      fprintf ppf "StExc(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      (ast_list ast_ctyp) lst1
      (ast_list ast_string) lst2
  | StExp (loc,e1) ->
      fprintf ppf "StExp(@[%a,@ %a@])"
      ast_loc loc
      ast_expr e1
  | StExt (loc,str,c1,lst) ->
      fprintf ppf "StExt(@[%a,@ %S,@ %a,@ %a@])"
      ast_loc loc
      str
      ast_ctyp c1
      (ast_list ast_string) lst
  | StInc (loc,me1) ->
      fprintf ppf "StInc(@[%a,@ %a@])"
      ast_loc loc
      ast_module_expr me1
  | StMod (loc,str,me1) ->
      fprintf ppf "StMod(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ast_module_expr me1
  | StRecMod (loc,lst) ->
      fprintf ppf "StRecMod(@[%a,@ %a@])"
      ast_loc loc
      (
        ast_list 
        ( fun ppf (str,mt1,me1) ->
          fprintf ppf "(@[%S,@ %a,@ %a@])"
          str
          ast_module_type mt1
          ast_module_expr me1
        )
      ) lst
  | StMty (loc,str,mt1) ->
      fprintf ppf "StMty(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      ast_module_type mt1
  | StOpn (loc,lst) ->
      fprintf ppf "StOpn(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst
  | StTyp (loc,lst) ->
      fprintf ppf "StTyp(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_type_decl) lst
  | StUse (loc,str,lst) ->
      fprintf ppf "StUse(@[%a,@ %S,@ %a@])"
      ast_loc loc
      str
      (
        ast_list 
        ( fun ppf (st1,loc) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_str_item st1
          ast_loc loc
        )
      ) lst
  | StVal (loc,b,lst) ->
      fprintf ppf "StVal(@[%a,@ %B,@ %a@])"
      ast_loc loc
      b
      (
        ast_list 
        ( fun ppf (p1,e1) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_patt p1
          ast_expr e1
        ) 
      ) lst
and ast_type_decl ppf ((loc,str),lst1,c1,lst2) =
  fprintf ppf "(@[(@[%a,@ %S@]),@ %a,@ %a,@ %a@])"
  ast_loc loc
  str
  (
    ast_list
    ( fun ppf (str,(b1,b2)) ->
      fprintf ppf "(@[%S,@ (@[%B,@ %B@])@])"
      str
      b1
      b2
    )
  ) lst1
  ast_ctyp c1
  (
    ast_list 
    ( fun ppf (c1,c2) ->
      fprintf ppf "(@[%a,@ %a@])"
      ast_ctyp c1
      ast_ctyp c2
    ) 
  ) lst2
and ast_class_type ppf ct =
  match ct with
    CtCon (loc,lst1,lst2) ->
      fprintf ppf "CtCon(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst1
      (ast_list ast_ctyp) lst2
  | CtFun (loc,c1,ct1) ->
      fprintf ppf "CtFun(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_ctyp c1
      ast_class_type ct1
  | CtSig (loc,opt,lst) ->
      fprintf ppf "CtSig(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_opt ast_ctyp) opt
      (ast_list ast_class_sig_item) lst
and ast_class_sig_item ppf cg =
  match cg with
    CgCtr (loc,c1,c2) ->
      fprintf ppf "CgCtr(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_ctyp c1
      ast_ctyp c2
  | CgDcl (loc,lst) ->
      fprintf ppf "CgDcl(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_class_sig_item) lst
  | CgInh (loc,ct1) ->
      fprintf ppf "CgInh(@[%a,@ %a@])"
      ast_loc loc
      ast_class_type ct1
  | CgMth (loc,str,b,c1) ->
      fprintf ppf "CgMth(@[%a,@ %S,@ %B,@ %a@])"
      ast_loc loc
      str
      b
      ast_ctyp c1
  | CgVal (loc,str,b,c1) ->
      fprintf ppf "CgVal(@[%a,@ %S,@ %B,@ %a@])"
      ast_loc loc
      str
      b
      ast_ctyp c1
  | CgVir (loc,str,b,c1) ->
      fprintf ppf "CgVir(@[%a,@ %S,@ %B,@ %a@])"
      ast_loc loc
      str
      b
      ast_ctyp c1
and ast_class_expr ppf ce =
  match ce with
    CeApp (loc,ce1,e1) ->
      fprintf ppf "CeApp(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_class_expr ce1
      ast_expr e1
  | CeCon (loc,lst1,lst2) ->
      fprintf ppf "CeCon(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_list ast_string) lst1
      (ast_list ast_ctyp) lst2
  | CeFun (loc,p1,ce1) ->
      fprintf ppf "CeFun(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_patt p1
      ast_class_expr ce1
  | CeLet (loc,b,lst,ce1) ->
      fprintf ppf "CeLet(@[%a,@ %B,@ %a,@ %a@])"
      ast_loc loc
      b
      (
        ast_list 
        ( fun ppf (p1,e1) ->
          fprintf ppf "(@[%a,@ %a@])"
          ast_patt p1
          ast_expr e1
        )
      ) lst
      ast_class_expr ce1
  | CeStr (loc,opt,lst) ->
      fprintf ppf "CeStr(@[%a,@ %a,@ %a@])"
      ast_loc loc
      (ast_opt ast_patt) opt
      (ast_list ast_class_str_item) lst
  | CeTyc (loc,ce1,ct1) ->
      fprintf ppf "CeTyc(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_class_expr ce1
      ast_class_type ct1
and ast_class_str_item ppf cr =
  match cr with
    CrCtr (loc,c1,c2) ->
      fprintf ppf "CrCtr(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_ctyp c1
      ast_ctyp c2
  | CrDcl (loc,lst) ->
      fprintf ppf "CrDcl(@[%a,@ %a@])"
      ast_loc loc
      (ast_list ast_class_str_item) lst
  | CrInh (loc,ce1,opt) ->
      fprintf ppf "CrInh(@[%a,@ %a,@ %a@])"
      ast_loc loc
      ast_class_expr ce1
      (ast_opt ast_string) opt
  | CrIni (loc,e1) ->
      fprintf ppf "CrIni(@[%a,@ %a@])"
      ast_loc loc
      ast_expr e1
  | CrMth (loc,str,b,e1,opt) ->
      fprintf ppf "CrMth(@[%a,@ %S,@ %B,@ %a,@ %a@])"
      ast_loc loc
      str
      b
      ast_expr e1
      (ast_opt ast_ctyp) opt
  | CrVal (loc,str,b,e1) ->
      fprintf ppf "CrVal(@[%a,@ %S,@ %B,@ %a@])"
      ast_loc loc
      str
      b
      ast_expr e1
  | CrVir (loc,str,b,c1) ->
      fprintf ppf "CrVir(@[%a,@ %S,@ %B,@ %a@])"
      ast_loc loc
      str
      b
      ast_ctyp c1
;;

(* Dump interfaces AST *)

let ast_interf lst =
  fprintf std_formatter "%a"
  ( 
    ast_list 
    ( fun ppf (sg,loc) ->
      fprintf ppf "(@[%a,@ %a@])"
      ast_sig_item sg
      ast_loc loc
    )
  ) lst
;;

(* Dump implementation AST *)

let ast_implem lst =
  fprintf std_formatter "%a"
  (
    ast_list 
    ( fun ppf (st,loc) ->
      fprintf ppf "(@[%a,@ %a@])"
      ast_str_item st
      ast_loc loc
    )
  ) lst
;;

(* Register Pcaml printer *)

Pcaml.print_interf := ast_interf
;;

Pcaml.print_implem := ast_implem
;;
