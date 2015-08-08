open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Ast
open Astprinter
open Scheduling 
open Transformast
open Astimperative

(* maps structure_items of the form : 

   let%node NAME (IN1,IN2,...) (OUT1, OUT2, ...) = 
    IN1 := OUT1;
    ...
  
*)
let lustre_mapper argv =
  { default_mapper with
    structure = fun mapper strs ->
      let transform_str_item str =  
       match str.pstr_desc with
      | Pstr_extension (({txt="node";_},PStr [s]),_) ->
        begin match s.pstr_desc with
          | Pstr_value (_,[v]) ->
            let _node = mk_node (v.pvb_pat) (v.pvb_expr) in
            let _node = transform_node _node in
            print_node Format.std_formatter _node; 
            let _node = schedule _node in
            let _impnode = compile_node _node in
            print_node Format.std_formatter _node;
            printml_node Format.std_formatter _impnode; 
            ([%stri let f x = x]::[%stri let () = ()]::[])
          | _ -> Error.syntax_error s.pstr_loc
        end
      | x -> [default_mapper.structure_item mapper str]
      in
      let new_list = List.map transform_str_item strs
          in List.fold_left (fun l x -> x@l) [] new_list
  }

let () = register "lustre" lustre_mapper 
