open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident
open Parsing_ocl
open Parsing_ast
open Parsing_ast_printer
open Scheduler
open Normalizing
open Sequential_ast
open Sequential_ast_printer
open Proof_printer
open Sequentialize
open Codegen
open Imperative_ast


(** Variables from command-line args **)
let verbose = ref false
let clocks = ref false
let lustre = ref false
let no_auto = ref false
let why = ref false
let nonalloc = ref false
let typing = ref false
let not_printed_wrapper = ref true
let just_clock = ref false
let main = ref ""
let outputs_env = ref []

(** Clocking env **)
let env = ref []
let mini_env = ref []

(** Printing funs **)
let print_steps _node _norm_node _sched_node =
   Format.fprintf
                Format.std_formatter
                "(** Parsed Node **) \n %a \n (** Normalized Node **) \n %a (** Scheduled Node **) : \n %a"
                print_node _node
                print_node _norm_node
                print_node _sched_node

let print_why node =
  let whyml = Proof_compiling.pcompile_cnode node in
  whyml_node Format.std_formatter whyml


(** Extracting OCaml code funs **)
let create_imperative_code node =
  let _seq_node = seq_node node outputs_env in
  if !verbose then print_s_node Format.std_formatter _seq_node;
  let str = tocaml_node _seq_node in
  if !typing then
    Print_type.print_type str;
  str

let create_functional_code (node:Imperative_ast2.imp_node) =
  let stri = if !main = string_of_pattern node.i_name then
      [Extracting2.tocaml_main node]
    else []
  in
  let str = Extracting2.tocaml_node node::stri in
  str

(** Function that maps structure_items of the form :

let%node NAME (IN1,IN2,...) ~return:(OUT1, OUT2, ...) =
  OUT1 = EQ1;
  ...
  OUTX = EQX

into classic Ocaml code

 **)

let create_node mapper str =
  match str.pstr_desc with
  | Pstr_extension (({txt="node";_},PStr [s]),_) ->
    begin match s.pstr_desc with
      | Pstr_value (_,[v]) ->
        let _node = mk_node v.pvb_pat v.pvb_expr in
        let _norm_node = if !no_auto then _node else normalize_node _node in
        let _sched_node = if !no_auto then _node else schedule _norm_node in
        begin
          if !verbose then print_steps _node _norm_node _sched_node;
          if !lustre then Lustre_printer.to_lustre_file _node;
          if !why then print_why _sched_node;
          mini_env := Miniclock.clock_node !mini_env _sched_node;
          if !just_clock then [%str ] else
            begin
              let (new_env,_cnode) = Clocking.clock_node !env _sched_node in
              env := new_env;
              if !clocks then Clocking_ast_printer.print_node
                  Format.std_formatter (_cnode,!verbose);
              let _icnode = Compiling_w_clocks.compile_cnode _cnode in
              if !verbose then Imperative_ast2.printml_node
                  Format.std_formatter _icnode;
              if not !nonalloc then create_functional_code _icnode
              else create_imperative_code _sched_node
            end
        end
      | _ -> Error.syntax_error s.pstr_loc "not a node"
    end
  | x -> [default_mapper.structure_item mapper str]

(** Custom mapper **)
let lustre_mapper argv =
  { default_mapper with structure =
                          fun mapper st ->
                            let stl = List.map (create_node mapper) st in
                            List.flatten stl }

(** Entry point **)
let _ =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  ("-y", Arg.Set why, "Prints whyml code");
                  ("-n", Arg.Set no_auto, "Don't normalize, don't schedule");
                  ("-l", Arg.Set lustre, "Prints lustre node");
                  ("-a", Arg.Set nonalloc, "Generate non-allocating code (state passing style)");
                  ("-m", Arg.Set_string main, "Generate main function");
                  ("-i", Arg.Set clocks, "Prints node clocks");
                  ("-clk", Arg.Set just_clock, "Just infer clocks, and stop");
                  ("-t", Arg.Set typing, "Prints node types");]
  in let usage_msg = "OCaLustre : "
  in Arg.parse speclist print_endline usage_msg;
  register "ocalustre" lustre_mapper
