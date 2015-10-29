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
open Lcd;;

let max_temp =   99;; (* 80.0°C *)
let def_temp =  654;; (* 32.5°C *)
let min_temp = 1010;; (*  2.0°C *)

let output = RD1;;
let plus_button = RD5;;
let minus_button = RD4;;

(* Acceleration de l'horloge du PIC *)
set_bit IRCF1;; set_bit IRCF0;; set_bit PLLEN;;

clear_bit RD1;;



write_reg TRISD 0b11111101;;
let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB;;

disp.init ();;
disp.config ();;
disp.register_bitmap '\000'
  0b01110_01010 0b01110_00000_00000 0b00000_00000_00000;;
disp.register_bitmap '\001'
  0b00000_00100 0b01000_10100_01000 0b00100_00000_00000;;
disp.register_bitmap '\002'
  0b00000_00100 0b00010_00101_00010 0b00100_00000_00000;;
disp.register_bitmap '\003'
  0b00000_10000 0b01000_10100_01000 0b10000_00000_00000;;
disp.register_bitmap '\004'
  0b00000_00001 0b00010_00101_00010 0b00001_00000_00000;;
disp.print_string "\00132.5\000C\002\n 32.5\000C ";;

module Option =
struct
  let get o = match o with
    | None -> failwith "None"
    | Some x -> x
end

let str_of_temp temp =
  if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else Printf.sprintf "%4.1f" (float_of_int (1033 - temp) /. 11.67)

let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES


let%node mode (plus, minus) (m)=
  m :=  0 --> 
        (if plus && not minus then 1 else
        if not plus && minus then 2 else
        if plus && minus then 3 else
        (pre m))


let%node thermo (plus,minus,def,t) (temp1, temp2, heat) = 
  temp1 := def --> new_temp1;
  mode := mode (plus,minus);
  new_temp1 := def --> (if plus then value_plus else (if minus then value_moins else (pre temp1) ) );
  value_plus := def --> (if mode = 1 then (pre temp1) -3 else (pre temp1) -1);
  value_moins := def --> (if mode = 2 then (pre temp1) +3 else (pre temp1) +1);
  temp2 := t;
  heat := true --> ( if ( t < temp1 - 3) then false else
                   if ( t > temp1 + 3) then true else
                   (pre heat) ) 


let _ =
  let main_step = thermo () in
  while true do
  let plus = test_bit plus_button in
  let minus = test_bit minus_button in
  let t = read_temp () in 
  let (temp1, temp2,heat) = main_step (plus,minus,def_temp,t) in
    disp.moveto 1 1;
    disp.print_string (str_of_temp temp1);
    disp.moveto 2 1;
    disp.print_string (str_of_temp temp2);
    if heat then set_bit output else clear_bit output;
    Sys.sleep 300;
  done
  
