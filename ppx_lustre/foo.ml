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


let%node truc () (a,b) =
  (a,b) := (1,4) -->( (pre a) * 4 , (pre b)*5)

let _ =
  while true do
    let (a,b) = truc () in 
    print_int a;
    print_string ",";
    print_int b;
    print_endline "";
    Unix.sleep 1
  done
  
