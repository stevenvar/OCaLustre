open Parsetree
open Asttypes
open Longident
open Ast

module M = Map.Make(String)

type clock = 
  Global | Clock of string | Constant 

(*
* AST with clocks as types 
*)

type c_node = {
 c_name : ident ;
 c_inputs : ident list;  
 c_outputs : ident list;
 c_equations : c_equation list; 
}
and 
 c_equation = { 
 c_pattern : c_pattern;
 c_expression : c_expression; 
} 
and 
  c_pattern =
  | C_Simple of ident * clock 
  | C_List of (ident * clock) list
and
  c_constant = Integer of int | Float of float | Bool of bool 
and
 c_expression = c_exp_desc * clock 
and
 c_exp_desc =   
  | C_Alternative of c_expression * c_expression * c_expression
  | C_Application of ident * c_expression list 
  | C_Call of Parsetree.expression
  | C_Application_init of ident * c_expression list
  | C_InfixOp of inf_operator * c_expression * c_expression
  | C_PrefixOp of pre_operator * c_expression
  | C_Value of constant 
  | C_Variable of ident
  | C_Tuple of c_expression list 
  | C_Ref of ident
  | C_When of c_expression * ident
  | C_Current of c_expression
  | C_Current_init of c_expression 
  | C_Unit



(*
* Errors 
*)

module Error = struct
  let print_error loc string =
    raise (Location.(Error(error ~loc:loc ("Error:"^string))))

  let syntax_error loc =
    print_error loc "Syntax Error"
end 

module Clocks = struct

  type t = clock M.t

  let empty = M.empty

  let add env x c = 
      M.add x c env

  let adds = 
    List.fold_left (fun env (x,c) -> add env x c)

  let find env x = 
  try 
    M.find x env
  with Not_found -> Global

end

let loc_default = Location.none

let get_clock ce1 ce2 = 
  match snd ce1, snd ce2 with
  | Global, Global -> Global 
  | Constant, x -> x
  | x , Constant -> x
  | Clock x, Clock y when (x=y) -> Clock x
  | _ -> failwith "the two expressions are not on the same clock "

let rec clock_expression e env = 
  match e with
  | Value v -> C_Value v, Constant
  | Variable v -> C_Variable v, (Clocks.find env v.content)
  | Ref i -> C_Ref i, (Clocks.find env i.content)
  | InfixOp (op,e1,e2) -> 
      let ce1 = clock_expression e1 env in 
      let ce2 = clock_expression e2 env in 
      let c = get_clock ce1 ce2 in
        C_InfixOp (op,ce1,ce2), c 
  | PrefixOp (op, e) -> 
      let ce = clock_expression e env in 
      C_PrefixOp (op, ce), (snd ce) 
  | When (e, i) -> 
      let ce = clock_expression e env in 
      C_When (ce,i) , (Clock i.content) 
  | Alternative (e1,e2,e3) ->
      let ce1 = clock_expression e1 env in 
      let ce2 = clock_expression e2 env in 
      let ce3 = clock_expression e3 env in 
      C_Alternative (ce1,ce2,ce3), get_clock ce2 ce3
  | Application_init (i, el) -> 
      let cel = List.map (fun e -> clock_expression e env) el  in 
      C_Application_init (i, cel), Global 
  | Application (i, el) -> 
    let cel = List.map (fun e -> clock_expression e env) el  in 
      C_Application (i, cel), Global 
  | Call e -> C_Call e, Global
  | Current e -> let ce = clock_expression e env in
    C_Current ce, Global
  | Current_init e -> let ce = clock_expression e env in
    C_Current_init ce, (snd ce )
  | _ -> C_Unit, Global 



let clock_pattern p c env = 
  match p with 
  | Simple i -> C_Simple (i,c), Clocks.add env i.content c 
  | List il -> C_List (List.map (fun i -> i,c) il), Clocks.adds env (List.map (fun i -> i.content,c) il)


let clock_equation env e = 
  let cexpr = clock_expression e.expression env in 
  let cpatt, new_env = clock_pattern e.pattern (snd cexpr) env in
  { c_pattern = cpatt; c_expression = cexpr }, new_env 


let rec clock_equations env eqs = 
  match eqs with
  | [] -> [], env  
  | e::t -> 
    let c_e, new_env = clock_equation env e in
    let c_eqs, new_env = clock_equations new_env t in 
    c_e :: c_eqs , new_env 

let clock_node n = 
  let cin = List.map (fun i -> (i.content, Global)) n.inputs in
  let cout = List.map (fun i -> (i.content, Global)) n.outputs in
  let env = Clocks.adds Clocks.empty cin in 
  let env = Clocks.adds env cout in 
  {
  c_name = n.name ;
  c_inputs = n.inputs;
  c_outputs = n.outputs; 
  c_equations = fst (clock_equations env n.equations);
  }

