open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Parsing_ocl
open Parsing_ast
open Parsing_ast_printer
open Clocking_ast
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
let alloc = ref false
let not_printed_wrapper = ref true
let outputs_env = ref []


(* maps structure_items of the form :

   let%node NAME ~i:(IN1,IN2,...) ~o:(OUT1, OUT2, ...) =
    OUT1 = IN1;
    ...
*)

let create_node mapper str =
  match str.pstr_desc with
  | Pstr_extension (({txt="node";_},PStr [s]),_) ->
    begin match s.pstr_desc with
      | Pstr_value (_,[v]) ->
        let _node = mk_node v.pvb_pat v.pvb_expr in
        let _norm_node = normalize_node _node in
        let _sched_node = schedule _norm_node in
        print_node Format.std_formatter _sched_node;
        let _seq_node = seq_node _sched_node outputs_env in
        print_s_node Format.std_formatter _seq_node;
        if !alloc then
          failwith "todo : put back code gen"
        else
          tocaml_node _seq_node
      | _ -> Error.syntax_error s.pstr_loc
    end
  | x -> [default_mapper.structure_item mapper str]


(* maps structure (i.e list of structure_items) *)
let lustre_mapper argv =

  { default_mapper with
    structure =
      fun mapper st ->
        let stl = List.map (create_node mapper) st in
        List.flatten stl
  }

let _ =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  ("-y", Arg.Set why, "Prints whyml code");
                  ("-m", Arg.Set alloc, "Generates allocating code");
                  ("-i", Arg.Set clocking, "Prints clocks types");]
  in let usage_msg = "OCaLustre : "
  in Arg.parse speclist print_endline usage_msg;
  register "ocalustre" lustre_mapper
