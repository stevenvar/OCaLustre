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

open Pic;;

let write_bit pin v =
  if v = 1 then set_bit pin else set_bit 0

let%node count (x) (a) =
  a := 0 next (pre a + 1)

let%node light () (pin) =
  pin := if count (0) > 10 then 1 else 0

let _ = 
  write_reg TRISB 0x00;
  while true do
    write_bit RB0 (light ());
    Sys.sleep 500;
  done
