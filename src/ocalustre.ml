open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Parsing_ocl
open Parsing_ast
open Parsing_ast_printer
open Clocking_ast

open Proof_compiling

(* open Scheduling *)
open Scheduler
open Normalizing
open Sequential_ast
open Sequential_ast_printer
open Proof_printer
open Sequentialize
open Codegen
open Error

let verbose = ref false
let clocking = ref false
let why = ref false

let not_printed_wrapper = ref true

let outputs_env = ref [] 

(* let typing_scheme_env = ref [] *)

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
            if !why && !not_printed_wrapper then
              (Format.fprintf Format.std_formatter
                 "module Test
       use import int.Int
       use import ref.Ref
       use import option.Option \n \n";
               not_printed_wrapper := false);

            let _node = mk_node v.pvb_pat v.pvb_expr in

            if !verbose then
              Format.fprintf Format.std_formatter
                " (* PARSED NODE *) \n %a" print_node _node;

            let _node = normalize_node _node in


            if !verbose then
            Format.fprintf Format.std_formatter
              " (* NORMALIZED NODE *) \n %a" print_node _node;

            let _node = schedule _node in

            if !verbose then
            Format.fprintf Format.std_formatter
              " (* SCHEDULED NODE *) \n %a" print_node _node;

            (* let (new_env, _cnode) = (clock_node _node typing_scheme_env clocking);
            in
            typing_scheme_env := new_env;

            if !verbose then
            Format.fprintf Format.std_formatter
              " (* CLOCKED NODE *) \n %a" print_cnode _cnode;
            *)
            (*  let ck_node = Clocking2.ck_node _node in *)

(* print_clock_scheme Format.std_formatter ck_node ; *)

             let _snode = seq_node _node outputs_env in
             print_s_node (Format.std_formatter) _snode;


            (* let _inode = compile_cnode _node in *)

            (* if !verbose then *)
            (* Format.fprintf Format.std_formatter *)
            (*   " (\* COMPILED NODE *\) \n %a" printml_node _inode; *)

            (* if !why then *)
            (*   (let _pnode = pcompile_cnode _node in *)
            (* Format.fprintf Format.std_formatter *)
            (*   " (\*  WHYML *\) \n\n%a\n\n\n" whyml_node _pnode *)
            (*   ); *)

             tocaml_node _snode


          | _ -> Error.syntax_error s.pstr_loc
        end

      | x -> default_mapper.structure_item mapper str
  }

let _ =

  register "ocalustre" lustre_mapper;

if !why then
    Format.fprintf Format.std_formatter
      "end \n \n ";
