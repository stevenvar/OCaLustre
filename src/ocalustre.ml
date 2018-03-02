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
open Compiling

let verbose = ref false
let clocking = ref false
let lustre = ref false
let why = ref false
let alloc = ref false
let not_printed_wrapper = ref true
let main = ref "" 
let outputs_env = ref []


(* maps structure_items of the form :

   let%node NAME (IN1,IN2,...) ~return:(OUT1, OUT2, ...) =
    OUT1 := EQ1;
    ...
    OUTX := EQX
*)

let to_lustre_file node =
  let name = !Location.input_name in
  let name = Filename.remove_extension name in
  let name = name^".ls" in
  let oc = open_out_gen [ Open_wronly; Open_creat ; Open_append] 0o640 name in
  let fmt =  Format.formatter_of_out_channel oc in
  Format.fprintf fmt "%a" Lustre_printer.print_node  node;
  close_out oc;
  Format.printf "File %s has been written for node %s. \n" name (string_of_pattern node.name)


let create_node mapper str =
  match str.pstr_desc with
  | Pstr_extension (({txt="node";_},PStr [s]),_) ->
    begin match s.pstr_desc with
      | Pstr_value (_,[v]) ->
        let _node = mk_node v.pvb_pat v.pvb_expr in
        let _norm_node = normalize_node _node in
        let _sched_node = schedule _norm_node in
          begin 
            if !verbose then
              print_node Format.std_formatter _sched_node;
            if !lustre then to_lustre_file _node;
            if !why then (
              let whyml = Proof_compiling.pcompile_cnode _sched_node in
              whyml_node Format.std_formatter whyml);
            if !clocking then (
              (* let _ = Clocker.clock_node _sched_node (ref []) (ref true) in *)
              Clocking_ocl.clock_node _sched_node
              (* Clocking_ocl.test (); *)
            );
            if not !alloc then
              begin 
                let _inode = compile_cnode _sched_node in
                let stri = if !main = string_of_pattern _inode.i_name then
                    [Extracting.tocaml_main _inode]
                  else
                    []
                in 
              Extracting.tocaml_node _inode::stri
              end
            else
              let _seq_node = seq_node _sched_node outputs_env in
              (* print_s_node Format.std_formatter _seq_node; *)
              tocaml_node _seq_node
          end
      | _ -> Error.syntax_error s.pstr_loc "not a node"
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
                  ("-l", Arg.Set lustre, "Prints lustre node");
                  ("-a", Arg.Set alloc, "Generate non-allocating code (state passing style)");
                  ("-main", Arg.Set_string main, "Generate main function");
                  ("-i", Arg.Set clocking, "Prints clocks types");]
  in let usage_msg = "OCaLustre : "
  in Arg.parse speclist print_endline usage_msg;
  register "ocalustre" lustre_mapper
