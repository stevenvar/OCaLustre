(*************************************************************************)
(*                                                                       *)
(*                                OCaPIC                                 *)
(*                                                                       *)
(*                             Benoit Vaugon                             *)
(*                                                                       *)
(*    This file is distributed under the terms of the CeCILL license.    *)
(*    See file ../../LICENSE-en.                                         *)
(*                                                                       *)
(*************************************************************************)

module Option = struct
  let get o = match o with
    | None -> failwith "non"
    | Some x -> x
end 


let%node edge (x) (y) = 
  y := false --> (x && x)

let%node entiers () (n) =
  n := 0 --> (pre n) + 1

let%node main () (a,b) = 
  a := entiers (); 
  b := entiers ()
  
let _ =
  let main_init = main () in 
  while true do
    let (a,b) = main_init () in 
    print_int a;
    print_int b; 
    print_endline "";
    Unix.sleep 1
  done
  
