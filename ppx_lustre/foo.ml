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

let%node bool_to_string (bool) (string) =
  string := if bool then "TRUE" else "FALSE"

let%node xor_writer (a,b) (x,s) =
   s:= bool_to_string (x);
   x:= if a then (not b) else b
let _ =
  while true do
    print_endline (snd (xor_writer (true,false)))
  done
  
