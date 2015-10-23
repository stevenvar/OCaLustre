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


let%node cpt () (n) = 
  n := 0 --> (pre n) + 1
  
let%node main () (a,b) =
  a := cpt ();
  b := cpt () 

let _ =
  Disp.init(); 
  Disp.config (); 
  while true do
  let (a,b) = main () in
    Disp.moveto 1 0;
    Disp.print_int a;
    Disp.moveto 1 4;
    Disp.print_int b; 
    Sys.sleep 1000;
  done
  
