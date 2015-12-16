open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast
open Ast_printer
open Scheduling 
open Transformast
open Ast_imperative
open Ast_clock
open Ast_clock_printer
open Ast_imperative_printer 
(* maps structure_items of the form : 

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
            let _node = transform_node _node in
            print_node Format.std_formatter _node;
            let _node = schedule _node in
            let _cnode = clock_node _node in 
            let _inode = compile_node _cnode in
            (*print_node Format.std_formatter _node; *)
            printml_node Format.std_formatter _inode; 
            print_cnode Format.std_formatter _cnode; 
            print_endline ""; 
            (tocaml_node _inode)
          | _ -> Error.syntax_error s.pstr_loc
        end
      | x -> default_mapper.structure_item mapper str
  }

let () = register "lustre" lustre_mapper
