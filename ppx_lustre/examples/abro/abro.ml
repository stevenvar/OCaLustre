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

 
(* Acceleration de l'horloge du PIC *)
set_bit IRCF1;; set_bit IRCF0;; set_bit PLLEN;;


module Disp = Lcd.Connect (
struct
  let bus_size = Lcd.Four
  let e  = LATD0
  let rs = LATD2
  let rw = LATD1
  let bus = PORTC
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

let print_bool b = 
  match b with
  | true -> Disp.print_string "t"
  | false -> Disp.print_string "f"

let%node abro (a,b,r) (o) = 
  o := seenA && seenB;
  seenA := false --> ( (not r) && (a || (pre seenA) ));
  seenB := false --> ( (not r) && (b || (pre seenB) ))


let _ =

  Disp.init(); 
  Disp.config (); 
  write_reg TRISB 0b00000111; 
  let main_step = abro () in
  while true do
   let valA = test_bit RB0 in
  let valB = test_bit RB1 in 
  let valR = test_bit RB2 in 
  let o = main_step (valA,valB,valR) in
    moveto 0 1;
    print_string "o=";
    print_bool o;
    Sys.sleep 50;
  done
  
