

open Pic
open Lcd


type kind = Nothing | Plus | Minus | Twice


let output = RD1
let plus_button = RD5
let minus_button = RD4

let max_temp =   99 (* 80.0°C *)
let def_temp =  654 (* 32.5°C *)
let min_temp = 1010 (* 2°C *)

let _ = 
	set_bit IRCF1;
 	set_bit IRCF0;	
  	set_bit PLLEN;
	clear_bit RD1;
	write_reg TRISD 0b11111101

let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB

module Option =
struct
  let get o = match o with
    | None -> failwith "None"
    | Some x -> x
end

let save_temp temp =
  Eeprom.write 0 (temp mod 256);
  Eeprom.write 1 (temp / 256);
  ()

let str_of_temp temp =
  if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else Printf.sprintf "%4.1f" (float_of_int (1033 - temp) /. 11.67)

let load_temp () =
  let temp = Eeprom.read 0 + Eeprom.read 1 * 256 in
  if temp < max_temp || temp > min_temp then def_temp else
  temp 

 let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES

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


let%node change_wtemp (state) (w) = 
	w :=  default --> ( 
			if state = 1 then pre w - 1 
	   else	if state = 10 then pre w - 3 
	   else	if state = 2 then pre w + 1
	   else if state = 20 then pre w  + 3
	   else pre w ) 

let%node thermo_on (state) (on) = 
	on := true --> (if state = 3 then not (pre on) else (pre on))

let%node heat (w,c) (h) = 
	h := false --> ( 
		 if (c < w - 3) then false 
	else if (c > w + 3) then true
	else pre h )

let%node save_t (w) (save) = 
	changed := false --> (w <> (pre w));
	save := if changed then call (save_temp w) else ()

let%node main (default, plus,minus,ctemp) (wtemp, on, heat) =
	state := call (buttons_state plus minus);
	on := thermo_on (state); 
	wtemp :=if on then change_wtemp (default, state) else 0; 
	heat := if on then heat (wtemp, ctemp) else false;
	save := save_t wtemp

let _ =
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
  let main_step = main () in
  while true do
  let plus = test_bit plus_button in
  let minus = test_bit minus_button in
  let t = read_temp () in 
  let ( wtemp, on,heat) = main_step (def_temp, plus,minus,t) in
    if on then (
    disp.moveto 1 1;
    disp.print_string (str_of_temp wtemp);
    disp.moveto 2 1;
    disp.print_string (str_of_temp t);
    if heat then set_bit output else clear_bit output;
	); 
    Sys.sleep 500;
  done


