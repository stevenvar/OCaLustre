
open Pic
open Lcd
type kind = Nothing | Plus | Minus | Twice

(** CONFIG **)

let output = RD1
let plus_button = RD5
let minus_button = RD4

let max_temp =   99 (* 80.0°C *)
let def_temp =  654 (* 32.5°C *)
let min_temp = 1010 (* 2°C *)

let config =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  write_reg TRISD 0b11111101;
  clear_bit RD1

let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB
(***)

let disp_inits =
  disp.init ();
  disp.config ();
  disp.register_bitmap '\000'
  0b01110_01010 0b01110_00000_00000 0b00000_00000_00000;
  disp.register_bitmap '\001'
  0b00000_00100 0b01000_10100_01000 0b00100_00000_00000;
  disp.register_bitmap '\002'
  0b00000_00100 0b00010_00101_00010 0b00100_00000_00000;
  disp.register_bitmap '\003'
  0b00000_10000 0b01000_10100_01000 0b10000_00000_00000;
  disp.register_bitmap '\004'
  0b00000_00001 0b00010_00101_00010 0b00001_00000_00000;
  disp.print_string "\00132.5\000C\002\n 32.5\000C "


let str_of_temp temp =
if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else
    let f = (float_of_int (1033 - temp) /. 11.67)  in
    Printf.sprintf "%4.1f" f

let save_temp temp = 
  Eeprom.write 0 (temp mod 256);
  Eeprom.write 1 (temp / 256);
  ()

let load_temp () = 
  let temp = Eeprom.read 0 + Eeprom.read 1 * 256 in
  if temp < max_temp || temp > min_temp then def_temp else
    temp 
  
let read_temp () = 
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO do () done;
  (read_reg ADRESH lsl 8) lor (read_reg ADRES)

                   
let counter = ref 0 
let kind = ref Nothing 
let buttons_state p m = 
  (* 	state = 
     1 : +
     10 : +++ 
     2 : - 
     20 : ---
     3 : on/off
     0 : nothing 
     *)
  match p , m, !kind, !counter with 
  | true , false, Nothing , 0 -> kind := Plus; incr counter; 1
  | true , false, Plus , 10 -> 10
  | true , false, Plus , _ -> incr counter; 1
  | false , true, Nothing, 0 -> kind := Minus; incr counter; 2
  | false , true, Minus, 10 -> 20
  | false , true, Minus, _ -> incr counter; 2
  | true , true, _, _ -> counter := 1; kind := Twice; 3
  | _ -> kind := Nothing; counter := 0 ; 0

let%node update_prop ~i:(wtemp,ctemp) ~o:(prop) =
  new_prop = 0 ->> ( call (min 100 (max 0 (!st_new_prop + offset)))); 
  delta = call (min 10 (max (-10) (ctemp-wtemp)));
  offset = call (min 10 (if delta < 0 then ( (-delta) * delta) else (delta * delta)) ); 
  prop = new_prop / 10


let%node timer ~i:(number) ~o:(alarm) =
  time = 1 ->> if (time) = 10 then 1 else (time) + 1;
  alarm = (time < number) 


let%node heat ~i:(w,c) ~o:(h) =
  prop = update_prop (w,c);
  h = timer (prop)


let%node change_wtemp ~i:(default,state) ~o:(w) = 
  w = default ->> ( 
      if state = 1 then w - 1 
      else	if state = 10 then w - 3 
      else	if state = 2 then w + 1
      else if state = 20 then w  + 3
      else w ) 

let%node thermo_on ~i:(state) ~o:(on) = 
  on = true ->> (if state = 3 then not (on) else (on))
(*
let%node heat (w,c) (h) = 
h := false --> ( 
if (c < w - 3) then false 
else if (c > w + 3) then true
else pre h )
*)
let%node save_t ~i:(w) ~o:(save) =
  pre_w = 0 ->> w; 
  changed = false ->> (w <> (pre_w));
  save = if changed then () (* call (save_temp w) *) else ()

let%node main ~i:(plus,minus,ctemp) ~o:(wtemp, on, h) =
  state = call ( buttons_state plus minus );
  on = thermo_on (state); 
  wtemp = if on then change_wtemp ( (call (load_temp ()) ),state) else 0; 
  h = if on then heat (wtemp, ctemp) else false;
  save = save_t wtemp


let _ =
  set_bit IRCF1;
  set_bit IRCF0;	
  set_bit PLLEN;
  clear_bit RD1;
  write_reg TRISD 0b11111101;
  
  disp.init ();
 disp.config (); 
  disp.register_bitmap '\000'
    0b01110_01010 0b01110_00000_00000 0b00000_00000_00000;
  disp.register_bitmap '\001'
    0b00000_00100 0b01000_10100_01000 0b00100_00000_00000;
  disp.register_bitmap '\002'
    0b00000_00100 0b00010_00101_00010 0b00100_00000_00000;
  disp.register_bitmap '\003'
    0b00000_10000 0b01000_10100_01000 0b10000_00000_00000;
  disp.register_bitmap '\004'
    0b00000_00001 0b00010_00101_00010 0b00001_00000_00000;
  disp.print_string "\00132.5\000C\002\n 32.5\000C ";
  let main_step = main (false,false,0) in
  while true do
    let plus = test_bit plus_button in
    let minus = test_bit minus_button in
    let t =  read_temp () in 
    let (wtemp, on,heat) = main_step (plus,minus,t) in
    if on then (
      disp.moveto 1 1;
      disp.print_string (str_of_temp wtemp);
      disp.moveto 2 1;
      disp.print_string (str_of_temp t);
      if heat then set_bit output else clear_bit output;
    ); 
    Sys.sleep 100;
  done


