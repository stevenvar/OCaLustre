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

let%node entiers () (n) =
  n := 0 --> (pre n) + 1
  
let _ =
  while true do
    print_int (entiers ());
    print_endline "";
    Unix.sleep 1
  done
  
