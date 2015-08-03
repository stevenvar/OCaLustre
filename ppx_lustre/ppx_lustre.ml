open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast
open Astprinter
open Scheduling 


(* maps structures of the form : 

   let%node NAME (IN1,IN2,...) (OUT1, OUT2, ...) = 
    IN1 := OUT1;
    ...
  
*)
let lustre_mapper argv =
  { default_mapper with
    structure_item = fun mapper str ->
      match str.pstr_desc with
      | Pstr_extension (({txt="node";_},PStr [s]),_) ->
        begin match s.pstr_desc with
          | Pstr_value (_,[v]) ->
            let _node = mk_node (v.pvb_pat) (v.pvb_expr) in
            print_node Format.std_formatter _node;  
            print_dependencies Format.std_formatter _node;
            [%stri let () = () ]
          | _ -> Error.syntax_error; assert false
        end
      | x -> default_mapper.structure_item mapper str
  }

let () = register "lustre" lustre_mapper 
