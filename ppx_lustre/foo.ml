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

let%node machin () (b) =
  b := 1 --> (pre b) * 5

let%node truc () (c) =
  c := 0 --> machin () 

let _ =
  while true do
    Format.fprintf Format.std_formatter "%d @." (truc ());
    Unix.sleep 1
  done
  
