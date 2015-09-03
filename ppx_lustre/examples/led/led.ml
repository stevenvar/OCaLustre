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

let moveto = Disp.moveto
let print_string = Disp.print_string


let%node print_h () (h) =
  h := print_string "h " 

let%node print_w () (w) =
  w := print_string "w "

let%node main () (a,b) =
  a := print_h ();
  b := print_w () 
  
  
let _ =
  while true do
    main ();
    Sys.sleep 1;
  done
  
