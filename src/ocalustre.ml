open Ast_mapper
open Asttypes
open Parsetree
open Parsing
open Parsing_ast
open Parsing_ast_printer
open Scheduler
open Sequential_ast
open Sequential_ast_printer
open Sequentialize
open Imperative_ast
open Clocking_ast


(** Variables from command-line args **)
let verbose = ref false
let clocks = ref false
let lustre = ref false
let wcet = ref false
let no_auto = ref false
let why = ref false
let nonalloc = ref false
let check = ref false
let delay = ref (-1)
let typing = ref false
let print_types = ref false
let just_clock = ref false
let main = ref ""
let outputs_env = ref []

(** Clocking environment **)
let clock_env = ref []

(** Typing environment **)
let typ_env = ref []

(** Printing functions: **)
let print_step_node title node = Format.fprintf
    Format.std_formatter
    "(** %s **)\n%a\n" title print_node node

let print_clocked_node node =
  Format.fprintf Format.std_formatter "(** Clocked Node **)\n%a\n"
    Clocking_ast_printer.print_node (node,!verbose)

let print_imperative_node node =
  Format.fprintf Format.std_formatter "(** Imperative Function **)\n%a\n"
    Imperative_ast.printml_node node

let print_why node =
  let whyml = Proof_compiling.pcompile_cnode node in
  Proof_printer.whyml_node Format.std_formatter whyml

(** Non-allocating code generation **)
let create_imperative_code node =
  let _seq_node = seq_node node outputs_env in
  if !verbose then print_s_node Format.std_formatter _seq_node;
  let str = Codegen.tocaml_node _seq_node (!main = string_of_pattern _seq_node.s_name) 0 !wcet in
  if !print_types then
    Print_type.print_type str;
  str

(** Functional code generation **)
let create_functional_code (node:Imperative_ast.imp_node) =
  let stri = if !main = string_of_pattern node.i_name then
      [Extracting.tocaml_main node !delay]
    else []
  in
  let str = Extracting.tocaml_node node::stri in
  str

(* Outputs imperative node as OCaml code: *)
let output_ocaml_code icnode cnode nonalloc =
  if nonalloc then create_imperative_code icnode else create_functional_code cnode

(* Checks correctness of clocking: *)
let check_clocks global_env local_env cnode =
  let checked = Check.check_node global_env local_env cnode in
  if (not checked) then Error.print_error cnode.cname.p_loc "Clock checking has failed";
  Format.printf "Checking %a: OK \n" print_pattern cnode.cname

(** Maps structure_items of the form :

    let%node NAME (IN1,IN2,...) ~return:(OUT1, OUT2, ...) =
    OUT1 = EQ1;
    ...
    OUTX = EQX

    into classic OCaml code

 **)
let generate_node_from_str_value v =
  let node = mk_node v.pvb_pat v.pvb_expr in
  if !verbose then print_step_node "Parsed Node" node;
  let norm_node = if !no_auto then node else Normalize.norm_node node in
  if !verbose then print_step_node "Normalized Node" norm_node;
  let sched_node = if !no_auto then node else schedule norm_node in
  if !verbose then print_step_node "Scheduled Node" sched_node;
  if !lustre then Lustre_printer.to_lustre_file node;
  if !why then print_why sched_node;
  (* mini_env := Miniclock.clock_node !mini_env _sched_node; *)
  if !typing then typ_env := Minitypes.typ_node !typ_env sched_node !clocks;
  let (global_env, local_env, cnode) = Clocking.clk_node !clock_env sched_node !clocks in
  if !verbose then print_clocked_node cnode;
  if !check then check_clocks global_env local_env cnode;
  clock_env := global_env;
  if !just_clock then [%str ] else
    (let icnode = Compiling.compile_cnode cnode in
     if !verbose then print_imperative_node icnode;
     output_ocaml_code cnode icnode !nonalloc)

let create_node mapper str =
  match str.pstr_desc with
  | Pstr_extension (({txt="node";_},PStr [s]),_) ->
    begin match s.pstr_desc with
      | Pstr_value (_,[v]) ->
        generate_node_from_str_value v
      | _ -> Error.syntax_error s.pstr_loc "Not a node"
    end
  | _ -> [default_mapper.structure_item mapper str]

(** Custom mapper for OCaLustre nodes **)
let ocalustre_mapper _argv =
  { default_mapper with
    structure = fun mapper st ->
      let stl = List.map (create_node mapper) st in List.flatten stl }

(** Entry point **)
let _ =
  let speclist = [("-v", Arg.Set verbose, "Enables verbose mode");
                  ("-n", Arg.Set no_auto, "Don't normalize, don't schedule");
                  ("-c", Arg.Set clocks, "Prints node clocks");
                  ("-t", Arg.Set typing, "Prints node type");
                  ("--why", Arg.Set why, "Prints whyml code");
                  ("--lustre", Arg.Set lustre, "Prints lustre node");
                  ("--nonalloc", Arg.Set nonalloc, "Generate non-allocating code (state passing style)");
                  ("--main", Arg.Set_string main, "Generate main function");
                  ("--delay", Arg.Set_int delay, "Set a main loop frequency (in ms delay)");
                  ("--clock", Arg.Set just_clock, "Just infer clocks, and stop");
                  ("--wcet", Arg.Set wcet, "Create bytecode compatible with bytecrawler");
                  ("--check-clocks", Arg.Set check, "Check clock inference");]
  in let usage_msg = "OCaLustre: "
  in Arg.parse speclist print_endline usage_msg;
  register "ocalustre" ocalustre_mapper
