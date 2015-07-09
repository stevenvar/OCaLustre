open Pcaml
open Token 
open MLast



let rec remove_exclam expr =
     let loc = MLast.loc_of_expr expr in 
     let se = remove_exclam in 
     let sel = List.map se in  
     match expr with
      <:expr< $e1$ . $e2$ >> -> <:expr< $e1$  >>
       | _ -> 
        Stdpp.raise_with_loc loc 
          (Failure 
             "syntax not supported due to the \
              lack of Camlp5 documentation")

let rec transform_expr exp = 
 let loc = MLast.loc_of_expr exp in 
 let se = transform_expr in
 let sel = List.map transform_expr in  
 match exp with 
  | <:expr< $e1$ := $e2$ >> ->         <:expr< write_bit $remove_exclam (e1)$ $se e2$ >>
  | <:expr< if $e1$ then $e2$ else $e3$ >> -> 
                                       <:expr< if $se e1$ then $se e2$ else $se e3$ >>
  | <:expr< $lid:s$ >> ->              exp
  | <:expr< do { $list:el$ } >> ->     <:expr< do { $list:sel el$ } >>
  | <:expr< $uid:s$ >> ->              (match s with 
					 "RA0" -> <:expr< test_bit $uid:s$ >>
				       | "RA1" -> <:expr< test_bit $uid:s$ >>
				       | "RA2" -> <:expr< test_bit $uid:s$ >>
				       | "RA3" -> <:expr< test_bit $uid:s$ >>
				       | "RA4" -> <:expr< test_bit $uid:s$ >>
				       | "RA5" -> <:expr< test_bit $uid:s$ >>
				       | "RA6" -> <:expr< test_bit $uid:s$ >>
				       | "RA7" -> <:expr< test_bit $uid:s$ >>
				       | "RB0" -> <:expr< test_bit $uid:s$ >>
				       | "RB1" -> <:expr< test_bit $uid:s$ >>
				       | "RB2" -> <:expr< test_bit $uid:s$ >>
				       | "RB3" -> <:expr< test_bit $uid:s$ >>
				       | "RB4" -> <:expr< test_bit $uid:s$ >>
				       | "RB5" -> <:expr< test_bit $uid:s$ >>
				       | "RB6" -> <:expr< test_bit $uid:s$ >>
				       | "RB7" -> <:expr< test_bit $uid:s$ >>
				       | "RC0" -> <:expr< test_bit $uid:s$ >>
				       | "RC1" -> <:expr< test_bit $uid:s$ >>
				       | "RC2" -> <:expr< test_bit $uid:s$ >>
				       | "RC3" -> <:expr< test_bit $uid:s$ >>
				       | "RC4" -> <:expr< test_bit $uid:s$ >>
				       | "RC5" -> <:expr< test_bit $uid:s$ >>
				       | "RC6" -> <:expr< test_bit $uid:s$ >>
				       | "RC7" -> <:expr< test_bit $uid:s$ >>
				       | "RD0" -> <:expr< test_bit $uid:s$ >>
				       | "RD1" -> <:expr< test_bit $uid:s$ >>
				       | "RD2" -> <:expr< test_bit $uid:s$ >>
				       | "RD3" -> <:expr< test_bit $uid:s$ >>
				       | "RD4" -> <:expr< test_bit $uid:s$ >>
				       | "RD5" -> <:expr< test_bit $uid:s$ >>
				       | "RD6" -> <:expr< test_bit $uid:s$ >>
				       | "RD7" -> <:expr< test_bit $uid:s$ >>
				       | "RE0" -> <:expr< test_bit $uid:s$ >>
				       | "RE1" -> <:expr< test_bit $uid:s$ >>
				       | "RE2" -> <:expr< test_bit $uid:s$ >>
				       | "RE3" -> <:expr< test_bit $uid:s$ >>
				       | "RE4" -> <:expr< test_bit $uid:s$ >>
				       | "RE5" -> <:expr< test_bit $uid:s$ >>
				       | "RE6" -> <:expr< test_bit $uid:s$ >>
				       | "RE7" -> <:expr< test_bit $uid:s$ >>
				       | _ -> exp)
					 
  | _ ->                               exp (* Stdpp.raise_with_loc loc (Failure " what is this ") *)

let transform (p,exp) =  
  (p, transform_expr exp)

let transform_list exp = List.map transform exp 
(*  | _ -> Stdpp.raise_with_loc loc (Failure "wrong syntax") *)
  

(* The statement that extends the default grammar, 
   i.e. the regular syntax of OCaml if we use camlp5o 
   or the revised syntax if we use camlp5r *)
EXTEND
(*  Pcaml.expr: LEVEL "top" [
    [ x = Pcaml.expr; o = OPT "rec";  "where"; l = LIST1 Pcaml.let_binding SEP "and"  ->
          <:expr< let $opt:o2b o$ $list:l$ in $x$ >> ]
  |
    [ "node"; e = LIST1 let_binding SEP "and";  "in";
        x = expr LEVEL "top" ->
          <:expr< let  $list:e$ in $x$ >> ]
  ];
  Pcaml.expr: LEVEL ":=" [
    [ e1 = SELF; "<-"; e2 = expr LEVEL "expr1" -> <:expr< set_bit $e1$ $e2$ >>  ]
 ]; *)

Pcaml.expr : LEVEL "top" [

        [ 
                "pre" ; x = Pcaml.patt LEVEL "simple" ->  <:expr< $x$_pre.val >>
                
        ]

];

Pcaml.expr : LEVEL "top" [

        [ 
                "node" ; e = LIST1 let_binding SEP "and"; "in" ; x = expr LEVEL "top" -> 
                <:expr<let $list:transform_list e$ in $x$ >>  
                
        ]

];

Pcaml.str_item : LEVEL "top" [

	[
	        "node"; l = LIST1 let_binding SEP "and" ->
		 match l with 
		 | [(<:patt< _ >>, e)] ->  <:str_item< $exp:e$ >>
		 | _ ->                    <:str_item< value $list: transform_list l$ >>
       ]
];

END;;
