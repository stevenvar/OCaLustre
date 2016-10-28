open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Parsing_ocl
open Parsing_ast
open Parsing_ast_printer
open Clocker
open Clocking_ast

open Clocking_ast_printer
(* open Scheduling *)
open Scheduler
open Normalizing
open Imperative_ast
open Imperative_ast_printer
    open Whyml_printer
open Compiling
open Extracting
open Error

let verbose = ref false
let clocking = ref false
let why = ref false 


let typing_scheme_env = ref []

(*open Transform
open Ast_imperative
open Ast_clock
open Ast_clock_printer
  open Ast_imperative_printer*)

(* maps structure_items of the form :

   let%node NAME ~i:(IN1,IN2,...) ~o:(OUT1, OUT2, ...) =
    OUT1 = IN1;
    ...
*)




let lustre_mapper argv =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  ("-y", Arg.Set why, "Prints whyml node");
                 ("-i", Arg.Set clocking, "Prints clocks types");]

    in let usage_msg = "OCaLustre : "
    in Arg.parse speclist print_endline usage_msg;
  { default_mapper with
    structure_item = fun mapper str ->
       match str.pstr_desc with
      | Pstr_extension (({txt="node";_},PStr [s]),_) ->
        begin match s.pstr_desc with
          | Pstr_value (_,[v]) ->


            let _node = mk_node (v.pvb_pat) (v.pvb_expr) in

            if !verbose then
            Format.fprintf Format.std_formatter
              " -- PARSED NODE -- \n %a" print_node _node;

            let _node = normalize_node _node in


            if !verbose then
            Format.fprintf Format.std_formatter
              " -- NORMALIZED NODE -- \n %a" print_node _node;


            let _node = schedule _node in


            if !verbose then
            Format.fprintf Format.std_formatter
              " -- SCHEDULED NODE -- \n %a" print_node _node;

            let (new_env, _cnode) = (clock_node _node typing_scheme_env clocking);
            in
            typing_scheme_env := new_env;

            if !verbose then
            Format.fprintf Format.std_formatter
              " -- CLOCKED NODE -- \n %a" print_cnode _cnode;


            let _inode = compile_cnode _node in

            if !verbose then
            Format.fprintf Format.std_formatter
              " -- COMPILED NODE -- \n %a" printml_node _inode;

            if !why then 
            Format.fprintf Format.std_formatter
              " -- WHYML -- \n\n%a\n\n\n" printwhyml_node _inode; 


            tocaml_node _inode

          | _ -> Error.syntax_error s.pstr_loc
        end

      | x -> default_mapper.structure_item mapper str
  }

let _ =



  register "ocalustre" lustre_mapper
