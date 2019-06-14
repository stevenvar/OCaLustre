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
open Sequentialize2
open Codegen
open Imperative_ast2
open Clocking_ast


(** Variables from command-line args **)
let verbose = ref false
let clocks = ref false
let lustre = ref false
let no_auto = ref false
let why = ref false
let nonalloc = ref false
let check = ref false
let delay = ref (-1)
let typing = ref false
let print_types = ref false
let not_printed_wrapper = ref true
let just_clock = ref false
let main = ref ""
let outputs_env = ref []

(** Clocking env **)
(* let env = ref [] *)
(* let mini_env = ref [] *)
let simpl_env = ref []

(** Typing env **)
let typ_env = ref []

(** Printing funs **)
let print_steps _node _norm_node _sched_node _cnode _icnode =
   Format.fprintf
                Format.std_formatter
                "(** Parsed Node **) \n %a \n (** Normalized Node **) \n %a (** Scheduled Node **) : \n %a \n (** Clocked Node **) \n %a \n (** Imperative Function **) \n %a"
                print_node _node
                print_node _norm_node
                print_node _sched_node
                Clocking_ast_printer.print_node (_cnode,!verbose)
                Imperative_ast2.printml_node _icnode

let print_why node =
  let whyml = Proof_compiling.pcompile_cnode node in
  whyml_node Format.std_formatter whyml


(** Extracting OCaml code funs **)
let create_imperative_code node =
  let _seq_node = seq_node node outputs_env in
  if !verbose then print_s_node Format.std_formatter _seq_node;
  Printf.printf "%s vs %s\n" !main (string_of_pattern _seq_node.s_name);
  let str = Codegen.tocaml_node _seq_node (!main = string_of_pattern _seq_node.s_name) 0 in
  if !print_types then
    Print_type.print_type str;
  str

let create_functional_code (node:Imperative_ast2.imp_node) =
    let stri = if !main = string_of_pattern node.i_name then
                 [Extracting2.tocaml_main node !delay]
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
        (* let _norm_node = if !no_auto then _node else normalize_node _node in *)
        let _norm_node = if !no_auto then _node else Normalize2.norm_node _node in
        if !verbose then Format.fprintf
                Format.std_formatter
                "%a"
                print_node _node;
        let _sched_node = if !no_auto then _node else schedule _norm_node in
        begin
          if !lustre then Lustre_printer.to_lustre_file _node;
          (* if !why then print_why _sched_node; *)
          (* mini_env := Miniclock.clock_node !mini_env _sched_node; *)
          if !typing then typ_env := Minitypes.typ_node !typ_env _sched_node !clocks;
          let (global_env, local_env, _cnode) = Clocking_ocl.clk_node !simpl_env _sched_node !clocks in
          let checked = if !check then (Check.check_node global_env local_env _cnode) else true in
          if !check then Format.printf "Checking of %a : %b \n" print_pattern _cnode.cname checked;
          if checked then
            begin
              simpl_env := global_env;
              if !just_clock then [%str ] else
                begin
                  let _icnode = Compiling_ocl.compile_cnode _cnode in
                  if !verbose then (
                    print_steps _node _norm_node _sched_node _cnode _icnode);
                  if not !nonalloc then create_functional_code _icnode
                  else create_imperative_code _cnode
                end
            end
          else
            Error.print_error _cnode.cname.p_loc "Clock checking has failed"
        end
      | _ -> Error.syntax_error s.pstr_loc "not a node"
    end
  | x -> [default_mapper.structure_item mapper str]

(** Custom mapper **)
let lustre_mapper argv =
  { default_mapper with
    structure = fun mapper st -> let stl = List.map (create_node mapper) st in List.flatten stl }

(** Entry point **)
let _ =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  (* ("-y", Arg.Set why, "Prints whyml code"); *)
                  ("-n", Arg.Set no_auto, "Don't normalize, don't schedule");
                  ("-l", Arg.Set lustre, "Prints lustre node");
                  ("-na", Arg.Set nonalloc, "Generate non-allocating code (state passing style)");
                  ("-m", Arg.Set_string main, "Generate main function");
                  ("-i", Arg.Set clocks, "Prints node clocks");
                  ("-t", Arg.Set typing, "Prints node type");
                  ("-d", Arg.Set_int delay, "Set a main loop frequency (in ms delay)");
                  ("-clk", Arg.Set just_clock, "Just infer clocks, and stop");
                  ("-check_clocks", Arg.Set check, "Check clock inference");
                  ("-t", Arg.Set print_types, "Prints node types");]
  in let usage_msg = "OCaLustre : "
  in Arg.parse speclist print_endline usage_msg;
  register "ocalustre" lustre_mapper
