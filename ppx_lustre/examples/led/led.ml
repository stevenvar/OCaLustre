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

open Pic

 

module Disp = Lcd.Connect (
struct
  let bus_size = Lcd.Four
  let e  = RC5
  let rs = RD3
  let rw = RC4
  let bus = PORTB
end
  )

module Option =
struct
  let get o = match o with
    | None -> failwith "None"
    | Some x -> x
end

let write_bit pin b =
  if b then set_bit pin else clear_bit pin

let%node switch () (value,pin) =
  value := true --> not (pre value);
  pin := if value then "ON" else "OFF"

let _ =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  Disp.init ();
  Disp.config ();
  while true do
    Disp.clear (); 
    Disp.print_string (snd (switch ()));
    Sys.sleep 500;
  done
