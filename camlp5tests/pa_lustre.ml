open Pcaml
open Token 
open MLast


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



Pcaml.str_item : LEVEL "top" [

	[
	        "node"; l = LIST1 let_binding SEP "and" ->
		 match l with 
		 | [(<:patt< _ >>, e)] ->  <:str_item< $exp:e$ >>
		 | _ ->                    <:str_item< value $list: transform_list l$ >>
       ]
];

END;;
